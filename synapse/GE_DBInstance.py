import os
import csv
import operator
import datetime
import functools
import sqlalchemy
import sqlalchemy.orm
import sqlalchemy.util
import sqlalchemy.exc
import sqlalchemy.orm.session
import sqlalchemy.orm.collections

from .GE_Exception import ConfigSchemaAliasException, ModelUnReachableException, ModelNameNotFoundException, AssociationStillInUseException, ModelGroupParentException, InstanceWriteError
from .GE_RowElement import DataCollection, DataLine, RowElement, DataBlock
from .GE_DataFunctions import ModelFunctionCatalog, InstanceFunctionCatalog
from .GE_Parsers import ExtendFormat, SynapseConditionParser, TendrilExpressionParser, TendrilObjectParser


class elemType:
	Key				=	0
	RecursiveKey	=	1
	Element			=	2
	Variable		=	3


class condType:
	SQLFilter		=	0
	Instruction		=	1


class joinType:
	InnerJoin		= 	0
	LeftOuter		=	1
	RightOuter		=	2


class PKNotFoundError(sqlalchemy.exc.SQLAlchemyError):
	pass


class TableClassType(type):
	pass


class InstanceHierarchyRoot:
	def __init__(self, child):
		self.child 			= child
		self.parent 		= None
		self.instance_node 	= None


def class_for_table(selectable, **mapper_kwargs):
	selectable = sqlalchemy.sql.expression._clause_element_as_expr(selectable)
	mapname = 'Mapped' + selectable.name.capitalize()
	klass = TableClassType(mapname, (object,), {})

	mappr = sqlalchemy.orm.mapper(	klass,
									selectable,
									allow_null_pks=False,
									**mapper_kwargs)

	return klass


class FOIL:
	'''
	Calculates an incremental transitive closure on a sparse representation of the problem space. It does this
	using the foil (first, outer, inner, last) method. Also houses the relational table structure for the source system.
	'''
	def __init__(self, url, dbschema):
		self.decendants   = {}
		self.ancestors	  = {}
		self.mapping	  = {}
		self.links		  = {}
		self.aliastables  = {}
		self.states       = {}

		try:
			self.engine       = sqlalchemy.create_engine(str(url))
			self.meta         = sqlalchemy.MetaData(self.engine)
			self.meta.reflect(schema=dbschema)
			#self.engine.echo  = True
		except sqlalchemy.exc.DBAPIError as e:
			print('%s\nSkipping database schema load of [%s] faking with sqlite[memory]' % (e.orig,url))
			self.engine       = sqlalchemy.create_engine('sqlite:///')
			self.meta         = sqlalchemy.MetaData(self.engine)

		self.SessionMaker = sqlalchemy.orm.sessionmaker( bind=self.engine )

	def instruction(self, text):
		self.engine.execute(text)

	def readblock ( self, allfields, joins, filters, distinct ):
		fields, groupby = [], []
		for field in allfields:
			col = self.aliastables[field[0]][1].columns[field[1]]
			if field[2]:
				col = getattr(sqlalchemy.sql.func,field[2]) ( col )
			else:
				groupby.append(col)
			fields.append(col)

		having_filters  = sqlalchemy.and_(*(x.parsedexpr for x in filters if x.having) )
		regular_filters = sqlalchemy.and_(*(x.parsedexpr for x in filters if not x.having) )

		query = sqlalchemy.sql.select(fields, from_obj=joins)

		if regular_filters:
			query = query.where(regular_filters)
		if len(groupby)!=len(fields):
			query = query.group_by(groupby)
			if having_filters:
				query = query.having(having_filters)
		if distinct:
			query = query.distinct()
		print('\n\n',query)
		return self.engine.execute(query).fetchall(), ['$'.join(x[:2]) for x in allfields]

	def writeblock ( self, backptr, alias, mode, block, targetsession ):

		def insert(cls, kwargs):
			o = cls()
			o.__dict__.update(kwargs)
			return o

		def delete(table, *args, **kwargs):
			table.delete(*args, **kwargs).execute()

		def createlinks(parent_alias, parentblock):
			localcache.setdefault(parent_alias, {})
			for link_position, (reftype, key_fields, data_fields, underblocks) in parentblock.Linkage.items():
				for underblock in underblocks.values():
					idparent 	= set([parentblock.KeyMap.keys()[y] for y in key_fields] + [parentblock.DataMap.keys()[y] for y in data_fields])
					idchild 	= set(underblock.KeyMap.keys())
					refind		= 1 if reftype=='tablerefby' else 0
					linkedalias = [ alias_pair [ 1 - refind ] for alias_pair, linkage in self.links.items() if parent_alias == alias_pair[ refind ] and set(linkage[0][ refind ])==idparent and set(linkage[0][ 1 - refind ] )==idchild ]

					if len(linkedalias)>1:
						if control['CreateIfEmpty']:
							raise InstanceWriteError('multiple possible destination aliases (%s) for target [%s]' % (','.join(linkedalias),parent_alias) )
						linkedalias = [x for x in linkedalias if set(underblock.DataMap.keys()).issubset( set(self.meta.tables[self.aliastables[x][0]].columns.keys()) ) ]

					if not linkedalias:
						#if this was a recursive object - fix this, the check needs to be stronger
						if underblock.DataMap==parentblock.DataMap and underblock.KeyMap==parentblock.KeyMap:
							localcache[parent_alias][(link_position,underblock.GetTypeName())] = (reftype, parent_alias, underblock)
							continue
						raise InstanceWriteError('can not link from alias ' + parent_alias)

					dest_alias								= linkedalias.pop()
					localcache[parent_alias][(link_position,underblock.GetTypeName())] = [reftype, dest_alias, underblock]

					createlinks ( dest_alias, underblock )

		def createtabledefinitions(orig_alias, parentblock):
			tablecolumns = tabledefs.setdefault( self.aliastables[orig_alias][0], {} ).setdefault('columns', sqlalchemy.util.OrderedSet() )
			existingcols = [x[0] for x in tablecolumns]
			for col,coltype in zip(parentblock.KeyMap.keys()+parentblock.DataMap.keys(), parentblock.keytype+parentblock.elemtype):
				if not coltype.startswith('table') and col not in existingcols:
					tablecolumns.add ( (col,coltype, col in parentblock.primaryseqfield, col in parentblock.sequences and coltype=='int' ) )
			for link_position, link_data in localcache[orig_alias].items():
				linktype, dest_alias, underblock 	= link_data[0], link_data[1], link_data[2]
				if orig_alias!=dest_alias:
					source, target	= (orig_alias, dest_alias) if linktype=='tablerefby' else (dest_alias, orig_alias)
					joins = zip(*[v for k,v in self.links.items() if source == k[1] and target == k[0]].pop()[0])
					tabledefs.setdefault( self.aliastables[source][0], {} ).setdefault('foreign', {}).update( dict([(v, '%s.%s' % ( self.aliastables[target][0], k) ) for k,v in joins]) )
					tabledefs.setdefault( self.aliastables[target][0], {} ).setdefault('relation',{}).setdefault('_Synapse_children_%s_%s' % (source, target), {'table': self.aliastables[source][0],
																																								'primaryjoin': joins,
																																								'backref':'_Synapse_parent_%s_%s' % (target,source),
																																								'remote':dict(joins).keys() if self.aliastables[source][0]==self.aliastables[target][0] else [] } )
					createtabledefinitions(dest_alias, underblock)

		def checktablestatus(tabledefinitions):
			for tablename, tabledata in tabledefinitions.items():
				table = self.meta.tables[tablename]
				if control['CreateIfEmpty']:
					columns 	= []
					drop_table 	= False
					for columndata in tabledata['columns']:
						col, coltype, isprimary, issequence = columndata[0], columndata[1], control['CreateContraints'] and columndata[2], control['CreateSequence'] and columndata[3]
						if col not in table.c:
							isforeign	= control['CreateContraints'] and col in tabledata.get('foreign',[])
							coln		= sqlalchemy.Column ( col, hackytypemap[coltype], sqlalchemy.Sequence(col+'_seq', optional=True ), primary_key=isprimary ) if issequence \
										  else ( sqlalchemy.Column ( col, hackytypemap[coltype], sqlalchemy.ForeignKey(tabledata['foreign'][col]), index=True) if isforeign else sqlalchemy.Column ( col, hackytypemap[coltype], primary_key=isprimary ) )

							if isprimary:
								drop_table = True
							columns.append(coln)

					if 	drop_table:
						table.drop()
					for coln in columns:
						table.append_column ( coln )
						if not drop_table: coln.create(table)

				if '_Synapse_backptr' not in table.c and control['CreateBackPtr']:
					coln = sqlalchemy.Column('_Synapse_backptr', sqlalchemy.Integer)
					table.append_column(coln)
					coln.create(table)

				if not table.primary_key.columns:
					raise PKNotFoundError('table %r does not have a primary key defined [columns: %s]' % (tablename, ','.join(table.c.keys())))

			self.meta.create_all()

			classes = {}
			for tablename, tabledata in tabledefinitions.items():
				table 				= self.meta.tables[tablename]
				origclass			= classes.setdefault (tablename, class_for_table ( table ) )
				if 'relation' in tabledata:
					props				= {}
					for relationname, relationdata in tabledata['relation'].items():
						desttable 	= self.meta.tables[ relationdata['table'] ]
						destclass	= classes.setdefault ( relationdata['table'], class_for_table ( desttable ) )
						join		= sqlalchemy.and_( * [table.columns[k]==desttable.columns[v] for k,v in dict(relationdata['primaryjoin']).items()] )
						if relationdata['remote']:
							sqlalchemy.orm.class_mapper(origclass)._configure_property(relationname, sqlalchemy.orm.relation (destclass, primaryjoin = join, backref=sqlalchemy.orm.backref( relationdata['backref'], remote_side = [ desttable.columns[k] for k in relationdata['remote'] ] ) ) )
						else:
							sqlalchemy.orm.class_mapper(origclass)._configure_property(relationname, sqlalchemy.orm.relation (destclass, primaryjoin = join, backref=sqlalchemy.orm.backref( relationdata['backref'] ) ) )
			return classes

		def writerow(mapperclasses, orig_alias, key, dataline, prev_record):
			for link_position, (reftype, key_fields, data_fields, underblock) in dataline.parent.Linkage.items():
				linked_keys 						= tuple( [ key[i] for i in key_fields ] + [ dataline[i] for i in data_fields ] )
				linked_block 						= dataline[link_position]
				reftypec, dest_alias, underblock 	= localcache[orig_alias][(link_position,linked_block.parent.GetTypeName())]
				dest_table							= self.aliastables[dest_alias][0]

				for child_line in linked_block:
					if reftype == 'tablerefby':
						recurse 		= linked_keys not in visited.setdefault(dest_table,{})
						linked_record 	= visited.setdefault(dest_table,{}).setdefault(linked_keys, insert ( mapperclasses [dest_table],
															dict ( ( [('_Synapse_backptr', backptr.id)] if control['CreateBackPtr'] else [] ) +
															zip ( underblock.KeyMap.keys(), linked_keys ) +
															child_line.getRecord() ) ) )
						shouldmerge		= control['Merge'] and not linked_block.parent.primaryseqfield
						if orig_alias==dest_alias:
							refind		= 0
							orig_alias  = [ alias_pair [ 1 - refind ] for alias_pair, linkage in self.links.items() if orig_alias == alias_pair[ refind ]  ].pop()
						#setattr(prev_record, '_Synapse_parent_%s_%s' % (dest_alias, orig_alias), linked_record)
						getattr(linked_record, '_Synapse_children_%s_%s' % (orig_alias, dest_alias ) ).append( session.merge(prev_record) if shouldmerge else prev_record )
						if recurse:
							writerow (mapperclasses, dest_alias, linked_keys, child_line, linked_record )
					else:
						linked_record	= insert ( mapperclasses [dest_table],
												dict ( ( [('_Synapse_backptr', backptr.id)] if control['CreateBackPtr'] else [] ) +
												zip ( underblock.KeyMap.keys(), linked_keys ) +
												child_line.getRecord() ) )
						shouldmerge		= control['Merge'] and not linked_block.parent.primaryseqfield
						if orig_alias==dest_alias:
							rfefind		= 1
							dest_alias  = [ alias_pair [ 1 - refind ] for alias_pair, linkage in self.links.items() if orig_alias == alias_pair[ refind ]  ].pop()
						#setattr(linked_record, '_Synapse_parent_%s_%s' % (orig_alias, dest_alias), prev_record)
						getattr(prev_record, '_Synapse_children_%s_%s' % (dest_alias, orig_alias) ).append( session.merge(linked_record) if shouldmerge else linked_record )
						writerow (mapperclasses, dest_alias, linked_keys, child_line, linked_record )

		hackytypemap 	= {'str':sqlalchemy.Text, 'int':sqlalchemy.Integer, 'real':sqlalchemy.Float}
		control 		= {'Merge':False, 'CreateIfEmpty':False, 'CreateSequence':True, 'CreateContraints':True, 'CreateBackPtr':False}

		if 'M' in mode:
			control['Merge']=True
		if 'C' in mode:
			control['CreateIfEmpty']=True
		if 'B' in mode:
			control['CreateBackPtr']=True

		localcache	= {}
		tabledefs	= {}
		visited		= {}

		createlinks 			(alias, block)
		createtabledefinitions	(alias, block)
		
		mapperclasses 	= checktablestatus ( tabledefs )
		session 		= targetsession if targetsession else self.SessionMaker()
		
		try:		
			for key,value in block.items():
				for dataline in value:
					row 	= insert ( mapperclasses [self.aliastables[alias][0]] , dict ( ( [('_Synapse_backptr',backptr.id)] if control['CreateBackPtr'] else [] ) + zip ( block.KeyMap.keys(), key ) + dataline.getRecord() ) )
					record 	= session.merge(row) if (control['Merge'] and not block.primaryseqfield) else row
					writerow (mapperclasses, alias, key, dataline, record)
					session.add(record)
			if not targetsession: session.commit()
		except InstanceWriteError as e:
			session.rollback()
			raise
		except PKNotFoundError as e:
			session.rollback()
			raise
##		except:
##			session.rollback()
		finally:
			if not targetsession: session.close()

	def pushalias(self, table, alias):
		if '$' in alias:
			raise ConfigSchemaAliasException(table, alias, "Aliases cannot have a '$' character")
		if table not in self.meta.tables:
			t = sqlalchemy.Table ( table, self.meta, sqlalchemy.Column('_Synapse_backptr', sqlalchemy.Integer ) )
			self.meta.create_all()
		self.aliastables.setdefault ( alias, ( table, self.meta.tables[table].alias() ) )

	def popalias(self, alias):
		# remove all links to this alias - might be a better way to do this - just remove the links that were added
		for link in [x for x in self.mapping.keys() if ( x[0]==alias or x[1]==alias )]:
			if link in self.mapping: self.poplink(link)
		for link in [x for x in self.links.keys() if ( x[0]==alias or x[1]==alias )]:
			del self.links[link]
		del self.aliastables[alias]

	def GetNewAlias(self, aliasName):
		'''quick and dirty way to generate a unique alias name'''
		suffix = 1
		while aliasName+'_'+str(suffix) in self.aliastables:
			suffix+=1
		return aliasName+'_'+str(suffix)

	def PushAliasState(self, aliasesToAdd, state):
		for oldalias, newalias in aliasesToAdd.items():
			if newalias not in self.aliastables:
				self.states.setdefault(state,{}).setdefault('aliases',[]).append(newalias)
				self.pushalias ( self.aliastables[ oldalias ][0] , newalias )

		# aliasesToAdd MUST have at least 2 aliases
		aliases = aliasesToAdd.keys()
		for alias1 in aliases[:-1]:
			for alias2 in aliases[1:]:
				aliaspair = ( (alias1, alias2) if (alias1, alias2) in self.links else (alias2, alias1) )
				if aliaspair in self.links:
					newaliaspair = ( aliasesToAdd[aliaspair[0]], aliasesToAdd[aliaspair[1]] )
					linkdata = self.links[ aliaspair ]
					for f1,f2 in zip(*linkdata[0]):
						self.pushlink ( newaliaspair, (f1, f2) + linkdata[1] )
					self.states.setdefault(state,{}).setdefault('linktypes',{}).setdefault(aliaspair, linkdata[-1])
					linkdata[-1] = (linkdata[-1][0], joinType.InnerJoin)

	def PopAliasState(self, state):
		for alias in self.states[state]['aliases']:
			self.popalias ( alias )
		for link_key,linkdata in self.states[state]['linktypes'].items():
			self.links[link_key][-1] = linkdata
		del self.states[state]

	def poplink(self, link):

		def updatelink(newlink, path):
			if path:
				self.mapping[newlink].update(path)
			else:
				del self.mapping[newlink]
				self.decendants[newlink[0]].discard(newlink)
				self.ancestors[newlink[1]].discard(newlink)

		i,j = link

		# if there's a path from i to j (via another table, we need to reflect this in all other links that have included i and j in it's shortest path)
		newpath = set()

		alldecendants = [ (i,x[1]) for x in self.decendants[j] if (i,x[1]) in self.mapping ]
		allancestors  = [ (x[0],j) for x in self.ancestors [i] if (x[0],j) in self.mapping ]

		for ancestor in ( x for x in allancestors if i not in self.mapping[x] ):
			for decendant in ( x for x in alldecendants if x[1]==ancestor[0] and j not in self.mapping[x] ):
				path = self.mapping[ancestor].union(self.mapping[decendant]).union( [ ancestor[0] ] )
				if len( path ) < len ( self.mapping[ link ] ) or len(self.mapping[link])==0:
					newpath				 = path
					self.mapping[ link ] = path

		#if there are no paths available from i to j then we have disconnected the graph and we need to delete links from the one component to the other.
		ancestors  = [ x for x in allancestors  if i in self.mapping[x] ]
		decendants = [ x for x in alldecendants if j in self.mapping[x] ]

		#notice the symmetry - nice.
		for ancestor in ancestors :
			for decendant in decendants :
				nolink = (ancestor[0], decendant[1])
				updatelink(nolink, newpath)

		for ancestor in ancestors :
			updatelink(ancestor, newpath)

		for decendant in decendants :
			updatelink(decendant, newpath)

		updatelink(link, newpath)

	def pushlink(self, link, fields, undirected=True):
		'''incremental update of mapping (guaranteed transitive closure after call)'''
		a, b = link

		self.links.setdefault( link, [ ([],[]), 1 ] )
		self.links[link][0][0].append(fields[0])
		self.links[link][0][1].append(fields[1])
		self.links[link][1] = (fields[2], fields[3])

		#self.links.setdefault( link, fields )

		for link in ([(a,b), (b,a)] if undirected else [(a,b)]):
			i,j 			   = link

			self.mapping[link] = set()
			self.ancestors.setdefault(j,set()).add(link)
			self.decendants.setdefault(i,set()).add(link)

			ancestors  = self.ancestors.get(i,[])
			decendants = self.decendants.get(j,[])

			for ancestor in ancestors:
				if ancestor[0]==j:
					continue

				newlink = (ancestor[0], j)

				self.mapping.setdefault( newlink, self.mapping[ancestor].union([i]) )

				self.decendants[ newlink[0] ].add( newlink )
				self.ancestors [ newlink[1] ].add( newlink )

				if len( self.mapping[ ancestor ] ) + 1 < len ( self.mapping[ newlink ] ):
					self.mapping[ newlink ] = self.mapping[ ancestor ].copy()
					self.mapping[ newlink ].add(i)

			for decendant in decendants:
				if decendant[1]==i:
					continue

				newlink = (i, decendant[1])

				self.mapping.setdefault ( newlink, self.mapping[decendant].union([j]) )

				self.decendants[ newlink[0] ].add( newlink )
				self.ancestors [ newlink[1] ].add( newlink )

				if len( self.mapping[ decendant ] ) + 1 < len ( self.mapping[ newlink ] ):
					self.mapping[ newlink ] = self.mapping[ decendant ].copy()
					self.mapping[ newlink ].add(j)

			for ancestor in ancestors:
				for decendant in decendants:
					if decendant[1]==ancestor[0]:
						continue

					newlink = (ancestor[0], decendant[1])

					self.mapping.setdefault(newlink, self.mapping[ ancestor ].union( self.mapping[ decendant ] ).union([i,j]))

					self.decendants[newlink[0]].add(newlink)
					self.ancestors[newlink[1]].add(newlink)

					if len(self.mapping[ ancestor]) + len ( self.mapping [ decendant ] ) + 2 < len(self.mapping[newlink]):
						self.mapping[newlink] = self.mapping[ ancestor].union(self.mapping[decendant])
						self.mapping[newlink].add(j)
						self.mapping[newlink].add(i)

	def GenerateJoins(self, aliases):

		def strongly_connected_components(graph):
			"""
			Find the strongly connected components in a graph using
			Tarjan's algorithm.
			
			graph should be a dictionary mapping node names to
			lists of successor nodes.
			"""

			result = []
			stack = []
			low = {}

			def visit(node):
				if node in low: return

				num = len(low)
				low[node] = num
				stack_pos = len(stack)
				stack.append(node)

				for successor in graph[node]:
					visit(successor)
					low[node] = min(low[node], low[successor])

				if num == low[node]:
					component = tuple(stack[stack_pos:])
					del stack[stack_pos:]
					result.append(component)
					for item in component:
						low[item] = len(graph)

			for node in graph:
				visit(node)

			return result

		def BuildAdjacencyMatrix(alias_set):
			'''
			Build's an adjacency matrix representing the connectivity of the given aliases subject to the schema loaded.
			Fills in any missing aliases required to reach all objects - throws an exception if the aliases are not in the same
			connected component
			'''

			nodes = list(alias_set)

			for i,v1 in enumerate(nodes[:-1]):
				for v2 in nodes[i+1:]:
					alias_set.update( self.mapping[(v1,v2)] )

			vertices = list(alias_set)
			edges = {}
			for link, value in self.links.items():
				if link[0] in alias_set and link[1] in alias_set:
					edges[link] = value

			A = dict.fromkeys(vertices)
			for alias in A.keys(): A[alias]=dict.fromkeys(vertices,-1)

			for key, val in edges.items():
				A[key[0]][key[1]] = val[1][0]
				A[key[1]][key[0]] = val[1][0]

			w = vertices.pop()
			for i in alias_set: A[i][w] = -1

			return alias_set, A, [w], []

		def prim(alias_set, A, w, path):
			'''
			Stock standard Prim's algorithm for finding the minimum spanning tree - should always work as we've already checked for connectivity
			'''

			if len(w) == len(alias_set):
				return path

			cost, vfrom, vto = min(functools.reduce(
				operator.concat, [[(val, v, key) for key, val in A[v].items() if val >= 0] for v in w]))
			w.append(vto)
			path.append((vfrom, vto) if (vfrom, vto) in self.links else (vto, vfrom))

			for i in alias_set: A[i][vto] = -1

			return prim(alias_set, A, w, path)

		def linkaliases(alias1, alias2):
			keys_1 = [self.aliastables[alias1][1].columns[y] for y in self.links[(alias1,alias2)][0][0]]
			keys_2 = [self.aliastables[alias2][1].columns[y] for y in self.links[(alias1,alias2)][0][1]]
			jointype = self.links[(alias1,alias2)][1][1]
			a1, a2, ky1, ky2 = (alias2, alias1, keys_2, keys_1) if jointype == joinType.RightOuter else (
				alias1, alias2, keys_1, keys_2)
			return (self.aliastables[a1][1], self.aliastables[a2][1], sqlalchemy.and_(
				* [k1 == k2 for k1,k2 in zip(ky1, ky2)]), jointype in (joinType.LeftOuter, joinType.RightOuter))

		components = strongly_connected_components(
			dict([(alias, [x[1] for x in self.mapping.keys() if x[0] == alias and x[1] in aliases]) for alias in aliases]))
		final_join = []

		for component in components:
			current_join = None
			partial_ordering = prim(* BuildAdjacencyMatrix(set(component)))

			if partial_ordering:
				current_join = sqlalchemy.join(* linkaliases(partial_ordering[0][0], partial_ordering[0][1]))
				visited = set(partial_ordering[0])
				for link in partial_ordering[1:]:
					alias1, alias2, on, jtype = linkaliases(link[0], link[1])
					current_join = current_join.join(
						alias2 if link[0] in visited else alias1, onclause=on, isouter=jtype)
					visited.update(link)

			if current_join:
				final_join.append( current_join )

		return final_join


class FileSchema:
	'''
	simple file abstraction
	'''
	def __init__(self, path):
		self.links			= {}
		self.aliastables	= {}
		self.fileparams     = {}
		self.path			= path

	def pushalias ( self, filename, alias ):
		self.aliastables.setdefault ( alias, filename )
		self.fileparams.setdefault(filename, {'delimiter':',', 'doublequote':csv.QUOTE_MINIMAL, 'escapechar':'\\', 'lineterminator':'\n'})
		#TODO - check filename and alias valid

	def popalias ( self, alias ):
		if len([x for x in self.fileparams if x==self.aliastables[alias]])==1:
			del self.fileparams[ self.aliastables[alias] ]
		del self.aliastables[alias]

	def readblock (self, allfields, joins, filters, distinct ):
		pass

	def writerows(self, filename, mode, data):
		with open(self.path+os.sep+filename, mode) as fileobj:
			options = self.fileparams[filename]
			if options['delimiter']:
				writer = csv.writer(fileobj, **options)
				writer.writerows(data.data)
			else:
				self.fileobj.writelines( ['%s\n' % (''.join([str(x) for x in line])) for line in data] )


	def instruction(self, text):
		pass

	def writeblock ( self, backptr, alias, mode, block, targetsession ):
		grid = ( 'Vertical', [ ( 'Horizontal', list( k ) + [ ( 'Vertical', ('Horizontal', list(v) ) ) ] ) for k,v in block.items() ] )
		self.writerows ( self.aliastables[alias], mode, ExtendFormat ( grid, [], 0 ) )

	def GenerateJoins( self, aliases ):
		pass

#ORM Classes
class TranLib(object):
	def __init__(self, **kwargs):
		self.__dict__.update(**kwargs)

class TranFunction(object):
	def __init__(self, **kwargs):
		self.__dict__.update(**kwargs)

class TranArg(object):
	def __init__(self, **kwargs):
		self.__dict__.update(**kwargs)

class ResourceParam(object):
	def __init__(self, **kwargs):
		self.__dict__.update(**kwargs)

class Alias(object):
	def __init__(self, **kwargs):
		self.__dict__.update(**kwargs)

class Link(object):
	def __init__(self, **kwargs):
		self.__dict__.update(**kwargs)

class InstanceHierarchy(object):
	def __init__(self, **kwargs):
		self.__dict__.update(**kwargs)

class InstanceRun(object):
	def __init__(self, **kwargs):
		self.__dict__.update(**kwargs)


class ConditionInstanceMap(object):
	def __init__(self, scope, model, override):
		self.id = None
		self.scope = scope
		self.model = model
		self.override = override
		self.next = None


class Condition(object):
	def __init__(self, name, address, type, value):
		self.id = None
		self.name = name
		self.address = address
		self.type = type
		self.value = value
		self.parsedexpr = None
		self.aliases = set()
		self.having = False

	@sqlalchemy.orm.reconstructor
	def init(self):
		self.Expression( self.address.GetParser() )

	def Expression(self, parser):
		if self.type==condType.SQLFilter:
			try:
				self.parsedexpr, self.aliases, self.having = parser.ParseCondition(self.value)
			except:
				print('Unable to Parse expression [%s] in address [%s]' % (self.value, self.address.name))


class Address(object):
	'''
	this needs to be abtracted to hold files/directories/xml/urls etc. - Currently only references relational databases
	TODO - needs to also allow the abstraction of mirroring the system tables alias & links sothat any change to one is reflected
	and updated in the other
	'''
	def __init__(self, name, addr_type, ext_addr, dbschema=None, is_system=False):
		self.name				= name
		self.address_type		= addr_type
		self.external_address   = ext_addr
		self.db_schema			= dbschema
		self.modelGroupRoot 	= ModelGroup(name, self, 'Root Group')
		self.LoadSchema()
		if is_system:
			sqlalchemy.orm.clear_mappers()
			MapTables(self.schema.meta.tables)

	@sqlalchemy.orm.reconstructor
	def LoadSchema(self):
		self.cond_psr 		= None
		if self.address_type in ['SQLite', 'Oracle', 'SQLServer', 'PostgreSQL', 'MySQL', 'Sybase']:
			self.schema = FOIL(self.external_address, self.db_schema)
		elif self.address_type == 'Directory':
			self.schema = FileSchema(self.external_address)

	def LoadParser(self, environment):
		self.cond_psr = SynapseConditionParser ( environment, self.schema, functionLibrary = ModelFunctionCatalog(environment) )

	def GetParser(self):
		return self.cond_psr

	def AddLink(self, link, fields):
		self.schema.pushlink(link, fields)

	def DelLink(self, link, fields):
		index = [ i for i,d in enumerate ( zip(*self.schema.links[link][0]) ) if d[0]==fields[0] and d[1]==fields[1] ]
		if index:
			self.schema.links[link][0][0].pop(index[0])
			self.schema.links[link][0][1].pop(index[0])
			if self.schema.links[link][0] == ([],[]):
				self.schema.poplink(link)
			return True
		else:
			#not found, it's an error fuck it.
			return False

	def IssueCommand(self, text):
		self.schema.instruction(text)

	def ReadDataBlock(self, allfields, joins, filters, distinct ):
		return self.schema.readblock(list(allfields), joins, filters, distinct)

	def WriteDataBlock(self, backptr, alias, mode, block, session=None):
		self.schema.writeblock(backptr, alias, mode, block, session)

	def SQLPushElementState(self, rowobj, elements, state):

		aliasesToAdd = {}
		#add the new key elements
		for recursivekey in rowobj.RecursiveKeyElements.values():
			keyelem 		= rowobj.KeyElements[ recursivekey.LinkedTo ]
			transitionlink 	= dict ( [ ( x[0], y[0] ) for x,y in zip(keyelem.fields, recursivekey.fields) ] )
			aliasesToAdd.update ( transitionlink )
			for alias1,alias2 in transitionlink.items():
				for linkingalias in self.schema.mapping[(alias1,alias2)]:
					aliasesToAdd.setdefault( linkingalias, self.schema.GetNewAlias( linkingalias ) )

		for elem in elements:
			for x in elem.fields: aliasesToAdd.setdefault( x[0], self.schema.GetNewAlias( x[0] ) )
			elem.exprStack = [ ( ( x[0], ( aliasesToAdd[x[1][0]], x[1][1], x[1][2] ) ) if x[0]=='Field' else x ) for x in elem.exprStack ]
			elem.updateExprStack()

		self.schema.PushAliasState ( aliasesToAdd, len(state) )

	def SQLPopElementState(self, state):
		self.schema.PopAliasState( len(state) )

	def Joins(self, aliases):
		return self.schema.GenerateJoins(aliases)


class Model(object):
	'''
	recursive container class
	'''
	def __init__( self, group, name, isdistinct, source ):

		self.name 		= name
		self.group		= group
		self.isdistinct = isdistinct
		self.source 	= source
		self._setup()

	@sqlalchemy.orm.reconstructor
	def _setup(self):
		self.RecursiveKeyElements 	= sqlalchemy.util.OrderedDict()
		self.KeyElements 			= sqlalchemy.util.OrderedDict()
		self.Elements 				= sqlalchemy.util.OrderedDict()
		self.Variables  			= {}

		#for the orm
		self.sequence	= []
		self.primary	= []
		self.submodels  = {}
		#the fields
		self.allfields  = set()
		#the aliases
		self.allaliases = set()

	def AddElement(self, elem, eType):
		if eType==elemType.Key:
			self.KeyElements[elem.name] = elem
		elif eType==elemType.RecursiveKey:
			self.RecursiveKeyElements[elem.name] = elem
		elif eType==elemType.Element:
			self.Elements[elem.name] = elem
		elif eType==elemType.Variable:
			self.Variables[elem.name] = elem
		else:
			#raise error
			pass

	def __repr__(self):
		return '[Model,%d,(%s),%s]' % (id(self), self.name, self.group.name)

	def __str__(self):
		return '[Model,%d,(%s),%s]' % (id(self), self.name, self.group.name)

	def Copy(self):
		copy 	   					= Model(self.group, self.name+'$', self.isdistinct, self.source)
		copy.RecursiveKeyElements 	= sqlalchemy.util.OrderedDict( (k,v.Copy()) for k,v in self.RecursiveKeyElements.items() )
		copy.Elements 				= sqlalchemy.util.OrderedDict( (k,v.Copy()) for k,v in self.Elements.items() )
		copy.KeyElements 			= sqlalchemy.util.OrderedDict( (k,v.Copy()) for k,v in self.KeyElements.items() )
		copy.Variables				= self.Variables.copy()
		return copy

	def Compile(self, instance, exec_stack):

		if not self.allfields:
			#process the fields and aliases
			for elem in (self.RecursiveKeyElements.values()+self.KeyElements.values()+self.Elements.values()+self.Variables.values()):
				self.allfields.update  ( elem.fields )

		#need to load up submodels
		SublevelRowObject 	= {}
		self.allaliases 	= set([x[0] for x in self.allfields])

		try:
			rows, meta = instance.Load( exec_stack )
		except ModelUnReachableException as e:
			#if the model is unreachable given the current context, catch it, add it's name and raise it
			raise ModelUnReachableException(e.args[0], self.name)
		print('ran ', self.name, ' numrows', len(rows))

		#now process the model again if recursive
		if self.RecursiveKeyElements and len(rows)>0:
			recrowobj = self.Copy()
			self.group.address.SQLPushElementState(self, recrowobj.Elements.values()+recrowobj.KeyElements.values()+recrowobj.RecursiveKeyElements.values()+recrowobj.Variables.values(), exec_stack)
			for submodelname, submodel in self.submodels.items():
				if submodelname != (self.name.replace('$',''),):
					virtualmodel						= submodel.Copy()
					recrowobj.submodels[submodelname] 	= virtualmodel
					self.group.address.SQLPushElementState(self, virtualmodel.Elements.values()+virtualmodel.KeyElements.values()+virtualmodel.Variables.values(), exec_stack)
			self.submodels[ (self.name.replace('$',''),) ] = recrowobj

		lenkeys, lendata 			= len(self.KeyElements), len(self.Elements)
		datablockDef				= ( (self.KeyElements.keys(), self.Elements.keys() ), ([x.dataType for x in self.KeyElements.values()], [x.dataType for x in self.Elements.values()]), self.primary, self.sequence )
		results 					= DataBlock ( * datablockDef )
		NullTuple 					= tuple([None]*lenkeys)

		#process subrowdata
		for name, obj in self.submodels.items():
			objdatablockDef			= ( (obj.KeyElements.keys(), obj.Elements.keys() ), ([x.dataType for x in obj.KeyElements.values()], [x.dataType for x in obj.Elements.values()]), obj.primary, obj.sequence )
			SublevelRowObject[name] = ( ( DataBlock( * objdatablockDef ) if len(rows)==0 else obj.Compile(instance, exec_stack+[obj] ) ) )

		#Context needs to be popped if this was a recursive object
		if self.RecursiveKeyElements and len(rows)>0:
			virtualmodel = self.submodels[(self.name.replace('$',''),)]
			for submodelname, submodelvalue in virtualmodel.submodels.items():
				if submodelname != (self.name.replace('$',''),):
					del submodelvalue.group.models[ submodelvalue.name ]
			del virtualmodel.group.models[ virtualmodel.name ]
			self.group.address.SQLPopElementState(exec_stack)

		for elem in self.KeyElements.values()+self.Elements.values()+self.Variables.values():
			elem.renumberType ( meta, self.KeyElements.keys()+self.Elements.keys() )

		#not sure about this again
		environment = self.group.address.GetParser().env
		environment.LocalParam[self.group.address.name] = 0
		environment.ResetLocals()

		for data in rows:
			localvars = dict( [ ( k, v.getVal(data, environment, SublevelRowObject ) ) for k,v in self.Variables.items() ] )
			values 	= [ x.getVal ( data, environment, SublevelRowObject, localvars ) for x in self.KeyElements.values() + self.Elements.values() ]

			keyValue = tuple ( item for item, elem in zip(values[:lenkeys], self.KeyElements.values()) if elem.getFormattedVal (item, values, environment) <= environment.Warning )
			if keyValue!=NullTuple and len(keyValue)==lenkeys:
				results.Add( keyValue, [ item for item, elem in zip(values[-lendata:], self.Elements.values()) if elem.getFormattedVal (item, values, environment) <= environment.Warning ] )
				environment.LocalParam[self.group.address.name]+=1

		#sort out linkage info
		for index, elem in enumerate(self.Elements.values()):
			if elem.dataType.startswith('table'):
				results.LinkKeys ( index, ( elem.dataType,
											[ self.KeyElements.keys().index(x[1]) for x in elem.fmtStack if x[1] in self.KeyElements ],
											[ self.Elements.keys().index(x[1]) for x in elem.fmtStack if x[1] in self.Elements ],
											elem.misc ) )
		return results


class ModelGroup(object):
	def __init__(self, name, address, description):
		self.id = None
		self.parent_id = None
		self.name = name
		self.address = address
		self.description = description

	def Append(self, node):
		node.parent = self
		self.children[node.name] = node

	def initmodel(self, alladdresses, modelname, keys, recursivekeys, switches, variables, attributes):

		def FindModel(startingpoint, name):
			#Buggy and error prone - please fix this in the next version
			try:
				if len(name)>2:
					startingpoint=alladdresses[name[0]].modelGroupRoot
					for i in name[1:-1]:
						if i in startingpoint.children:
							startingpoint = startingpoint.children[i]
				elif len(name)==2:
					startingpoint=startingpoint.parent.children[name[0]] if startingpoint.parent else startingpoint
				return 	startingpoint.models[ name[-1] ]
			except:
				raise ModelNameNotFoundException(name)

		model = self.models[modelname]
		allobjects = set()
		for attrib in attributes:
			if attrib[2] in keys:
				etype=elemType.Key
			elif attrib[2] in recursivekeys:
				etype=elemType.RecursiveKey
			else:
				etype=elemType.Element
			elem = self.CreateAttribute(attrib[0], attrib[1], attrib[2], attrib[3], attrib[4])
			model.AddElement(elem, etype)
			allobjects.update ( elem.objs )

		for var,vardata in variables.items():
			variable = self.CreateAttribute('var', 'Variable$', var, vardata, [])
			model.AddElement(variable, elemType.Variable)
			allobjects.update ( variable.objs )

		for key,rkey in zip(keys,recursivekeys):
			if key!=rkey:
				model.RecursiveKeyElements[rkey].LinkedTo = key

		model.submodels	= dict([(x,FindModel(self, x)) for x in allobjects])
		model.isdistinct = switches.get('select','').lower()=='distinct'
		model.primary    = switches.get('primary',[])
		model.sequence	 = switches.get('sequence',[])

	def __repr__(self):
		return self._getstring(0, False)

	def __str__(self):
		return self._getstring(0, False)

	def _getstring(self, level, expand = False):
		s = (' +' * level) + "%s (%s, %s, %s, %d)" % ( self.name, self.id, self.description, self.parent_id, id(self) ) + '\n'
		s+=''.join([('  ' * (level+1))+x+'\n' for x in self.models.keys()])
		if expand:
			s += ''.join([n._getstring(level+1, True) for n in self.children.values()])
		return s

	def print_nodes(self):
		return self._getstring(0, True)

	def AddModel(self, alladdresses, source, rowobjname, keys, recursivekeys, switches, variables, attributes):
		if rowobjname in self.models:
			self.models	[rowobjname]._setup()
			self.models	[rowobjname].source = source
		else:
			self.models[rowobjname] = Model( None, rowobjname, switches.get('select','all').lower()=='distict', source )
		self.initmodel(alladdresses, rowobjname, keys, recursivekeys, switches, variables, attributes)

	def CreateAttribute(self, dtype, alias, attr, stack, formattingStack):
		elem = RowElement(attr, alias, dtype)
		elem.exprStack = stack
		elem.fmtStack = formattingStack
		elem.updateExprStack()
		return elem


class Instance(object):
	def __init__(self, name, targetAddress=None, targetAlias=None, targetMode=None, serial=True, model=None):
		self.name 			    = name
		self.targetAddress		= targetAddress
		self.targetAlias_name	= targetAlias
		self.targetMode  		= targetMode
		self.serial				= serial
		self.model				= model

	@sqlalchemy.orm.reconstructor
	def setup(self):
		self.conditions = []
		self.sub_instances = []

	def init_children(self, session):
		node = session.query(ConditionInstanceMap).filter(sqlalchemy.and_(ConditionInstanceMap.next_id == None, ConditionInstanceMap.instance_id==self.id)).first()
		while node:
			self.conditions.insert(0, session.merge(node) )
			node=node.previous
		node = session.query(InstanceHierarchy).filter(sqlalchemy.and_(InstanceHierarchy.next_id == None, InstanceHierarchy.parent_id==self.id)).first()
		while node:
			self.sub_instances.insert(0, session.merge(node) )
			self.sub_instances[0].child.init_children(session)
			node=node.previous

	def __repr__(self):
		return self._getstring(0, False)

	def __str__(self):
		return self._getstring(0, False)

	def _getstring(self, level, expand = False, nextid = None):
		s =''.join([('  ' * (level+i))+x.child.name+'\n' for i,x in enumerate(self.conditions)])
		s += ('  ' * (level+len(self.conditions)+1)) + "%s (%s, Address:%s, Alias:%s, Mode:%s, Serial:%s, Model:%s, nextid:%s, %d)" % ( self.name, self.id, self.targetAddress_name, self.targetAlias_name, self.targetMode, self.serial, getattr(self.model,'name','N/A'), str(nextid.child_id if nextid else nextid ), id(self) ) + '\n'

		if expand:
			s += ''.join([n.child._getstring(level+len(self.conditions)+2, True, n.next) for n in self.sub_instances])
		return s

	def print_nodes(self):
		return self._getstring(0, True)

	def appendInstance(self, node, pos=0):
		a = InstanceHierarchy()
		a.child = node
		if pos>0:
			a.next = self.sub_instances[pos-1].next
			self.sub_instances[pos-1].next = a
		elif self.sub_instances:
			a.next = self.sub_instances[0]
		self.sub_instances.insert(pos, a)
		return a

	def removeInstance(self, node):
		before 			= node.previous
		after  			= node.next
		node.previous	= None
		node.next		= None
		if before:	before.next 	= after
		if after:	after.previous 	= before
		self.sub_instances.remove(node)
		del node

	def appendCondition(self, cond, scope, model, override, pos=0):
		a = ConditionInstanceMap(scope, model, override)
		a.child = cond
		if pos>0:
			a.next = self.conditions[pos-1].next
			self.conditions[pos-1].next = a
		elif self.conditions:
			a.next = self.conditions[0]
		self.conditions.insert(pos, a)
		return a

	def removeCondition(self, cond):
		before 			= cond.previous
		after  			= cond.next
		cond.previous	= None
		cond.next		= None
		if before:	before.next 	= after
		if after:	after.previous 	= before
		self.conditions.remove(cond)
		del cond

	def GetConditions(self, exec_stack ):

		def getrules():
			ret 	= []
			cap 	= False
			recs    = [x for x in exec_stack if x.RecursiveKeyElements]
			depth 	= len([x for x in exec_stack if not x.RecursiveKeyElements]) if recs else len(exec_stack)-1
			for x in self.stack[::-1]:
				if x.child.address==exec_stack[-1].group.address and ( ( x.model in exec_stack and x.scope>=depth-exec_stack.index(x.model) and not cap ) or ( x.model==None and x.scope>=depth and not cap ) ):
					x.child.init()
					ret.append(x.child)
					cap = x.override
			return ret[::-1]

		additionalAliases, res = set(), []

		filters = getrules()
		for condition_rule in filters:
			res.append( condition_rule )
			additionalAliases.update(condition_rule.aliases)
		return res, additionalAliases

	def Load(self, exec_stack):	#aka read
		model 						= exec_stack[-1]
		filters, conditionAliases	= self.GetConditions( exec_stack )
		try:
			joins = model.group.address.Joins ( model.allaliases.union( conditionAliases ) )
			return model.group.address.ReadDataBlock ( model.allfields, joins, filters, model.isdistinct )
		except sqlalchemy.exc.OperationalError as e:
			#no active db
			return [None], [None]

	def Execute(self, conditionstack, readdata=False):

		runlog = InstanceRun(start=datetime.datetime.now(), end=datetime.datetime.now(), instance=self)
		self.run_logs.append(runlog)

		#so we have an id for the runlog
		sqlalchemy.orm.session.Session.object_session(self).flush()

		if self.model:
			self.stack	= [ x for x in conditionstack+self.conditions if x.child.type==condType.SQLFilter ]
			commandHist	= [ x.child.address.IssueCommand(x.child.value) for x in conditionstack+self.conditions if x.child.type==condType.Instruction ]
			global data
			data = self.model.Compile ( self, [self.model] )
			self.targetAddress.WriteDataBlock( runlog, self.targetAlias_name, self.targetMode, data, sqlalchemy.orm.session.Session.object_session(self) if self.targetAddress_name=='System$' else None)
		else:
			for subinstance in self.sub_instances:
				runlog.children.append( subinstance.child.Execute ( conditionstack+self.conditions ) )

		runlog.end = datetime.datetime.now()
		return runlog


def CreateDB(metadata):
	Tables = {
	'TranLibrary': sqlalchemy.Table( 'TranLibrary', metadata,
						sqlalchemy.Column('id', sqlalchemy.Integer, sqlalchemy.Sequence('TranLibrary_id_seq', optional=True), primary_key=True),
						sqlalchemy.Column('libName', sqlalchemy.Text),
						sqlalchemy.Column('libOS', sqlalchemy.Text),
						sqlalchemy.Column('libPath', sqlalchemy.Text),
						sqlalchemy.Column('libType', sqlalchemy.Text),
						sqlalchemy.UniqueConstraint('libName', 'libOS', name='TranLibrary_uix_1')
						),
	'TranFunction': sqlalchemy.Table( 'TranFunction', metadata,
						sqlalchemy.Column('id', sqlalchemy.Integer, sqlalchemy.Sequence('TranFunction_id_seq', optional=True), primary_key=True),
						sqlalchemy.Column('funcName', sqlalchemy.Text),
						sqlalchemy.Column('lib_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('TranLibrary.id')),
						sqlalchemy.Column('docString', sqlalchemy.Text),
						sqlalchemy.Column('retType', sqlalchemy.Integer)
						),
	'TranArg': sqlalchemy.Table( 'TranArg', metadata,
						sqlalchemy.Column('id', sqlalchemy.Integer, sqlalchemy.Sequence('TranArg_id_seq', optional=True), primary_key=True),
						sqlalchemy.Column('function_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('TranFunction.id')),
						sqlalchemy.Column('argname',  sqlalchemy.Text),
						sqlalchemy.Column('argType',  sqlalchemy.Integer),
						sqlalchemy.Column('next_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('TranArg.id'))
					),
	'Address': sqlalchemy.Table(  'Address', metadata,
						sqlalchemy.Column('name', sqlalchemy.Text, primary_key=True),
						sqlalchemy.Column('address_type', sqlalchemy.Text),
						sqlalchemy.Column('external_address', sqlalchemy.Text),
						sqlalchemy.Column('db_schema', sqlalchemy.Text, nullable=True)
					),
	'Condition': sqlalchemy.Table( 'Condition', metadata,
						sqlalchemy.Column('id', sqlalchemy.Integer, sqlalchemy.Sequence('Condition_id_seq', optional=True), primary_key=True),
						sqlalchemy.Column('name', sqlalchemy.Text),
						sqlalchemy.Column('address_name', sqlalchemy.Text, sqlalchemy.ForeignKey('Address.name')),
						sqlalchemy.Column('type', sqlalchemy.Integer), # filter or instruction - ie where x>10 (Or loop 40 etc)
						sqlalchemy.Column('value', sqlalchemy.Text), #this may evolve to some kind of reverse polish expression object
						sqlalchemy.UniqueConstraint('name', 'address_name', name='Condition_uix_1')
					),
	'ModelGroup': sqlalchemy.Table( 'ModelGroup', metadata,
					sqlalchemy.Column('id', sqlalchemy.Integer, sqlalchemy.Sequence('Group_id_seq', optional=True), primary_key=True),
					sqlalchemy.Column('name', sqlalchemy.Text, index=True),
					sqlalchemy.Column('address_name', sqlalchemy.Text, sqlalchemy.ForeignKey('Address.name')),
					sqlalchemy.Column('description', sqlalchemy.Text),
					sqlalchemy.Column('parent_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('ModelGroup.id'), nullable=True),
					sqlalchemy.UniqueConstraint('name', 'parent_id', name='ModelGroup_uix_1')
					),
	'Model': sqlalchemy.Table('Model', metadata,
					sqlalchemy.Column('id', sqlalchemy.Integer, sqlalchemy.Sequence('Model_id_seq', optional=True), primary_key=True),
					sqlalchemy.Column('group_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('ModelGroup.id')),
					sqlalchemy.Column('name', sqlalchemy.Text),
					sqlalchemy.Column('isdistinct', sqlalchemy.Boolean),
					sqlalchemy.Column('source', sqlalchemy.Text),
					sqlalchemy.UniqueConstraint('group_id', 'name', name='Model_uix_1')
					),
	'ResourceParam': sqlalchemy.Table ( 'ResourceParam', metadata,
							sqlalchemy.Column('address_name', sqlalchemy.Text, sqlalchemy.ForeignKey('Address.name'), primary_key=True),
							sqlalchemy.Column('name', sqlalchemy.Text, primary_key=True),
							sqlalchemy.Column('autoIncField', sqlalchemy.Text )
							),
	'Alias': sqlalchemy.Table ('Alias', metadata,
					sqlalchemy.Column('address_name', sqlalchemy.Text, sqlalchemy.ForeignKey('Address.name'), primary_key=True),
					sqlalchemy.Column('name', sqlalchemy.Text, primary_key=True),
					sqlalchemy.Column('resource_name', sqlalchemy.Text, sqlalchemy.ForeignKey('ResourceParam.name'))
					),
	'Link': sqlalchemy.Table ('Link', metadata,
					sqlalchemy.Column('address_name', sqlalchemy.Text, sqlalchemy.ForeignKey('Address.name'), primary_key=True),
					sqlalchemy.Column('alias1_name', sqlalchemy.Text, sqlalchemy.ForeignKey('Alias.name'), primary_key=True),
					sqlalchemy.Column('field1', sqlalchemy.Text, primary_key=True),
					sqlalchemy.Column('alias2_name', sqlalchemy.Text, sqlalchemy.ForeignKey('Alias.name'), primary_key=True),
					sqlalchemy.Column('field2', sqlalchemy.Text, primary_key=True),
					sqlalchemy.Column('cost', sqlalchemy.Integer),
					sqlalchemy.Column('jointype', sqlalchemy.Integer, nullable=False)
				),
	'Instance': sqlalchemy.Table ( 'Instance', metadata,
					   sqlalchemy.Column('id', sqlalchemy.Integer, sqlalchemy.Sequence('Instance_id_seq', optional=True), primary_key=True),
					   sqlalchemy.Column('name', sqlalchemy.Text, index=True, nullable=False ),
					   sqlalchemy.Column('targetAddress_name', sqlalchemy.Text, sqlalchemy.ForeignKey('Address.name'), nullable=True),
					   sqlalchemy.Column('targetAlias_name', sqlalchemy.Text, sqlalchemy.ForeignKey('Alias.name'), nullable=True),
					   sqlalchemy.Column('targetMode', sqlalchemy.Text, nullable=True), #function describing how data is to be written (needs to be parsed)
					   sqlalchemy.Column('serial', sqlalchemy.Boolean, nullable=False),
					   sqlalchemy.Column('model_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('Model.id'), nullable=True)
					),
	'InstanceHierarchy': sqlalchemy.Table ( 'InstanceHierarchy', metadata,
								sqlalchemy.Column('id', sqlalchemy.Integer, sqlalchemy.Sequence('InstanceHierarchy_id_seq', optional=True), primary_key=True),
								sqlalchemy.Column('parent_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('Instance.id'), index=True),
								sqlalchemy.Column('child_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('Instance.id'), index=True),
								sqlalchemy.Column('next_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('InstanceHierarchy.id'), index=True, nullable=True)
								),
	'ConditionInstanceMap': sqlalchemy.Table ( 'ConditionInstanceMap', metadata,
								sqlalchemy.Column('id', sqlalchemy.Integer, sqlalchemy.Sequence('ConditionMap_id_seq', optional=True), primary_key=True),
								sqlalchemy.Column('condition_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('Condition.id')),
								sqlalchemy.Column('instance_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('Instance.id')),
								sqlalchemy.Column('scope', sqlalchemy.Integer), #at the highest level 0 means all, -1 means all but last, 1 means only this, 2 means this and next in line etc.
								sqlalchemy.Column('model_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('Model.id'), nullable=True), #specifically target this model
								sqlalchemy.Column('override', sqlalchemy.Boolean, nullable=True), #specifically target this model
								sqlalchemy.Column('next_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('ConditionInstanceMap.id'), index=True, nullable=True),
								sqlalchemy.UniqueConstraint('condition_id', 'instance_id', name='ConditionMap_uix_1')
							),
	'InstanceRun': sqlalchemy.Table ('InstanceRun', metadata,
						 sqlalchemy.Column('id', sqlalchemy.Integer, sqlalchemy.Sequence('InstanceRun_id_seq', optional=True), primary_key=True),
						 sqlalchemy.Column('start', sqlalchemy.DateTime, nullable=False),
						 sqlalchemy.Column('end', sqlalchemy.DateTime, nullable=False),
						 sqlalchemy.Column('instance_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('Instance.id'), index=True),
						 sqlalchemy.Column('parent_id', sqlalchemy.Integer, sqlalchemy.ForeignKey('InstanceRun.id'), nullable=True)
						 )
	}

def MapTables(Tables):
	conditionmapper = sqlalchemy.orm.mapper(Condition, Tables['Condition'])
	modelmapper 	= sqlalchemy.orm.mapper(Model, Tables['Model'])
	Aliasmapper		= sqlalchemy.orm.mapper(Alias, Tables['Alias'])
	Linkmapper		= sqlalchemy.orm.mapper(Link, Tables['Link'])
	Resoucemapper   = sqlalchemy.orm.mapper(ResourceParam, Tables['ResourceParam'])
	tranformlibmapper = sqlalchemy.orm.mapper ( TranLib, Tables['TranLibrary'], properties={
									'functions': sqlalchemy.orm.relation(TranFunction,
														 primaryjoin = Tables['TranLibrary'].c.id == Tables['TranFunction'].c.lib_id,
														 collection_class=sqlalchemy.orm.collections.attribute_mapped_collection('funcName'),
														 backref=sqlalchemy.orm.backref("library", remote_side=[Tables['TranLibrary'].c.id]))
									}
								)

	tranformFuncmapper = sqlalchemy.orm.mapper ( TranFunction, Tables['TranFunction'], properties={
									'arguments': sqlalchemy.orm.relation(TranArg,
														 primaryjoin = Tables['TranFunction'].c.id == Tables['TranArg'].c.function_id,
														 collection_class=sqlalchemy.orm.collections.attribute_mapped_collection('argName'))
									}
								)

	tranformArgsmapper = sqlalchemy.orm.mapper ( TranArg, Tables['TranArg'], properties={
									'next': sqlalchemy.orm.relation(TranArg, uselist=False,
														 remote_side=Tables['TranArg'].c.id,
														 primaryjoin = Tables['TranArg'].c.next_id==Tables['TranArg'].c.id,
														 backref=sqlalchemy.orm.backref('previous',uselist=False))
									}
								)
	addressmapper = sqlalchemy.orm.mapper ( Address, Tables['Address'], properties={
									'conditions': sqlalchemy.orm.relation(Condition, cascade="all, delete-orphan",
														 primaryjoin = Tables['Condition'].c.address_name == Tables['Address'].c.name,
														 collection_class=sqlalchemy.orm.collections.attribute_mapped_collection('name'),
														 backref=sqlalchemy.orm.backref('address',uselist=False)),
									'resourceparams': sqlalchemy.orm.relation(ResourceParam, cascade='all, delete-orphan',
														primaryjoin = Tables['ResourceParam'].c.address_name == Tables['Address'].c.name,
														collection_class=sqlalchemy.orm.collections.attribute_mapped_collection('name'),
														backref=sqlalchemy.orm.backref('address',uselist=False)),
									'aliases':  sqlalchemy.orm.relation(Alias, cascade='all, delete-orphan',
														primaryjoin = Tables['Alias'].c.address_name == Tables['Address'].c.name,
														collection_class=sqlalchemy.orm.collections.attribute_mapped_collection('name'),
														backref=sqlalchemy.orm.backref('address',uselist=False)),
									'links' : sqlalchemy.orm.relation(Link, cascade='all, delete-orphan',
														primaryjoin = Tables['Link'].c.address_name == Tables['Address'].c.name,
														collection_class=sqlalchemy.orm.collections.column_mapped_collection([Tables['Link'].c.alias1_name, Tables['Link'].c.field1, Tables['Link'].c.alias2_name, Tables['Link'].c.field2]),
														backref=sqlalchemy.orm.backref('address',uselist=False)),
									'groups' : sqlalchemy.orm.relation(ModelGroup, cascade='all, delete-orphan',
														primaryjoin = Tables['ModelGroup'].c.address_name == Tables['Address'].c.name,
														backref=sqlalchemy.orm.backref('address',uselist=False))
									}
								)

	instancemapper = sqlalchemy.orm.mapper ( Instance, Tables['Instance'], properties = {
									'conditions' : sqlalchemy.orm.relation(ConditionInstanceMap, cascade='all, delete-orphan',
													primaryjoin = Tables['Instance'].c.id == Tables['ConditionInstanceMap'].c.instance_id,
													backref=sqlalchemy.orm.backref('instance',remote_side=[Tables['Instance'].c.id]), lazy=None),
									'sub_instances' :sqlalchemy.orm.relation(InstanceHierarchy, cascade="all, delete-orphan",
													primaryjoin = Tables['Instance'].c.id == Tables['InstanceHierarchy'].c.parent_id,
													backref=sqlalchemy.orm.backref("instance_node", remote_side=[Tables['Instance'].c.id]), lazy=None),
									'run_logs'	: sqlalchemy.orm.dynamic_loader(InstanceRun, backref=sqlalchemy.orm.backref('instance',uselist=False)),
									'model' : sqlalchemy.orm.relation(Model, uselist=False,
													   primaryjoin = Tables['Instance'].c.model_id == Tables['Model'].c.id),
									'targetAddress' : sqlalchemy.orm.relation(Address, uselist=False,
														primaryjoin = Tables['Instance'].c.targetAddress_name == Tables['Address'].c.name)
									}
								)

	associationmapper = sqlalchemy.orm.mapper ( ConditionInstanceMap, Tables['ConditionInstanceMap'], properties = {
									'child' 		: sqlalchemy.orm.relation(Condition,
													primaryjoin = Tables['Condition'].c.id == Tables['ConditionInstanceMap'].c.condition_id),
									'model' : sqlalchemy.orm.relation(Model, uselist=False,
													   primaryjoin = Tables['ConditionInstanceMap'].c.model_id == Tables['Model'].c.id),
									'next': sqlalchemy.orm.relation(ConditionInstanceMap, uselist=False, post_update=True,
													remote_side=Tables['ConditionInstanceMap'].c.id,
													primaryjoin = Tables['ConditionInstanceMap'].c.next_id==Tables['ConditionInstanceMap'].c.id,
													backref=sqlalchemy.orm.backref('previous', uselist=False))
									}
								)

	hierarchymapper = sqlalchemy.orm.mapper( InstanceHierarchy, Tables['InstanceHierarchy'], properties = {
									'child' : sqlalchemy.orm.relation (Instance,
														 primaryjoin = Tables['Instance'].c.id == Tables['InstanceHierarchy'].c.child_id),
									'parent': sqlalchemy.orm.relation (Instance,
														 primaryjoin = Tables['Instance'].c.id == Tables['InstanceHierarchy'].c.parent_id),
									'next': sqlalchemy.orm.relation(InstanceHierarchy, uselist=False, post_update=True,
													remote_side=Tables['InstanceHierarchy'].c.id,
													primaryjoin = Tables['InstanceHierarchy'].c.next_id==Tables['InstanceHierarchy'].c.id,
													backref=sqlalchemy.orm.backref('previous', uselist=False))
									}
								)

	instancerunmapper = sqlalchemy.orm.mapper( InstanceRun, Tables['InstanceRun'], properties = {
									'children': sqlalchemy.orm.relation(InstanceRun, cascade="all",
													primaryjoin = Tables['InstanceRun'].c.parent_id == Tables['InstanceRun'].c.id,
													backref=sqlalchemy.orm.backref("parent", remote_side=[Tables['InstanceRun'].c.id]),
													lazy=False, join_depth=3)
									}
								)

	groupmapper = sqlalchemy.orm.mapper ( ModelGroup, Tables['ModelGroup'], properties = {
								'children': sqlalchemy.orm.relation(ModelGroup, cascade="all",
													primaryjoin = Tables['ModelGroup'].c.parent_id == Tables['ModelGroup'].c.id,
													collection_class=sqlalchemy.orm.collections.attribute_mapped_collection('name'),
													backref=sqlalchemy.orm.backref("parent", remote_side=[Tables['ModelGroup'].c.id]),
													lazy=False, join_depth=3),
								'models': sqlalchemy.orm.relation(Model, cascade='all, delete-orphan',
												   primaryjoin = Tables['ModelGroup'].c.id == Tables['Model'].c.group_id,
												   collection_class=sqlalchemy.orm.collections.attribute_mapped_collection('name'),
												   backref = sqlalchemy.orm.backref("group")
												   )
								}
						)


class Context(object):
	'''Context has a link to the environment and sql connection(s) to the source(s) of the mapping.
	   god only knows how this works.	
	'''
	def __init__(self, environment, functionLibrary, instanceLibrary):
		#need models to load up the context - the context is now split between the 2.
		self.environment			= environment
		self.InstanceRoot			= None
		self.Addresses 				= {}
		#the parsers . . .
		self.cond_psr		= None
		self.instance_psr 	= TendrilExpressionParser(functionLibrary = instanceLibrary, parseCondition=False)
		self.obj_psr 		= TendrilObjectParser(functionLibrary)
		self.session 		= None

	def init_addresses(self, session):
		def initmodels(group):
			for key, value in group.models.items():
				group.initmodel( self.Addresses, * self.obj_psr.ParseObject(value.source) )
			for subgroup in group.children.values():
				initmodels(subgroup)

		for address in session.query(Address).all():
			self.Addresses[address.name]=address
			address.LoadParser(self.environment)
			#process links and stuff here.
			for alias, data in address.aliases.items():
				address.schema.pushalias(data.resource_name, alias)
			for (alias1, field1, alias2, field2), data in address.links.items():
				address.AddLink( (alias1, alias2), (field1, field2, data.cost, data.jointype) )
			address.modelGroupRoot = session.query(ModelGroup).filter(sqlalchemy.and_(ModelGroup.parent_id==None,ModelGroup.address_name==address.name)).one()
			initmodels(address.modelGroupRoot)

	def init_instances(self, session):
		self.InstanceRoot = session.query(Instance).filter(sqlalchemy.and_(~sqlalchemy.sql.exists().where(InstanceHierarchy.child_id==Instance.id),
																		   sqlalchemy.sql.exists().where(InstanceHierarchy.parent_id==Instance.id))).first()
		if not self.InstanceRoot:
			self.InstanceRoot = session.query(Instance).first()
		if self.InstanceRoot: self.InstanceRoot.init_children(session)

	def LoadSystem( self, systemdb ):
		'''
		Create a memory database for storing the context - ideally should be in memory and then dump to disk.
		When trying to load another system, we must amend this one
		'''
		sysadd  = Address('System$', 'SQLite', systemdb, is_system=True)
		session = sysadd.schema.SessionMaker()

		self.init_addresses(session)
		self.init_instances(session)

		#set the system address for this context
		self.sysadd  = self.Addresses['System$']
		self.session = session

	def GetInstanceRoot(self):
		return InstanceHierarchyRoot(self.InstanceRoot)

	def Save(self):
		self.session.commit()
		if self.InstanceRoot: self.InstanceRoot.init_children(self.session)

	def Undo(self, full=True):
		self.session.expunge_all()
		self.session.rollback()
		if full:
			self.init_addresses(self.session)
			self.init_instances(self.session)

	def NeedsSaving(self):
		return self.session.new or self.session.dirty or self.session.deleted

	def AddAddress(self, name, addr_type, ext_addr, dbschema=None):
		if name in self.Addresses:
			raise Exception("Can't update existing address - must remove then insert with new name")
		else:
			self.Addresses[name] = Address(name, addr_type, ext_addr, dbschema)
			self.Addresses[name].LoadParser(self.environment)
			self.session.add( self.Addresses[name] )

	def DeleteAddress(self, name):
		if name in self.Addresses:
			self.session.delete( self.Addresses[name] )
			del self.Addresses[name]

	def AddAlias(self, addr, alias, resource):
		address = self.Addresses[addr]
		if alias in address.aliases:
			address.schema.popalias(alias)
			address.aliases[alias].resource_name = resource
		else:
			address.aliases[alias] = Alias(address=address, name=alias, resource_name=resource)
		address.schema.pushalias(resource, alias)

	def DeleteAlias(self, addr, alias):
		address = self.Addresses[addr]
		address.schema.popalias(alias)
		del address.aliases[alias]

	def AddAssociation(self, addr, alias1, alias2, field1, field2, cost, jointype):
		address = self.Addresses[addr]
		address.links[(alias1,field1,alias2,field2)]=Link(address=address, alias1_name=alias1, field1=field1, alias2_name=alias2, field2=field2, cost=cost, jointype=jointype)
		self.Addresses[addr].AddLink( (alias1, alias2), (field1, field2, cost, jointype) )

	def DeleteAssociation(self, addr, alias1, alias2, field1, field2):
		address = self.Addresses[addr]
		if address.DelLink( (alias1, alias2), (field1, field2) ):
			del address.links[ (alias1,field1,alias2,field2) ]
		else:
			raise AssociationStillInUseException("can't remove association (%s.%s = %s.%s) - try removing something else first" % (alias1, field1, alias2, field2) )

	def AddCondition(self, addr, name, type, value):
		address = self.Addresses[addr]
		if name in address.conditions:
			address.conditions[name].type = type
			address.conditions[name].value = value
		else:
			address.conditions[name] = Condition(name, address, type, value)

		address.conditions[name].Expression(address.GetParser())

	def DeleteCondition(self, addr, name):
		del self.Addresses[addr].conditions[name]

	def AddModelGroup(self, parent, name, desc):
		if parent==None:
			raise ModelGroupParentException(name)
		elif name in parent.children:
			parent.children[name].description = desc
		else:
			parent.children[name] = ModelGroup(name, parent.address, desc)
		return parent.children[name]

	def DeleteModelGroup(self, parent, name):
		if parent:
			grouptodie = parent.children[name]
			grouptodie.address.groups.remove(grouptodie)
			del parent.children[name]
		else:
			raise ModelGroupParentException(name)

	def AddModel(self, source, group):
		err = None
		obj = self.obj_psr.ParseObject(source)
		group.AddModel( self.Addresses, source, *obj )
		return obj[0]

	def DeleteModel(self, name, group):
		del group.models[name]

	def LinkConditionToInstance(self, condition, instance, scope, model, override, position=0):
		if condition in [x.child for x in instance.conditions]:
			node 			= [x for x in instance.conditions if x.child==condition][0]
			node.scope		= scope
			node.model  	= model
			node.override 	= override
			return 			node
		else:
			return instance.appendCondition( condition , scope, model, override, position )

	def UnlinkConditionFromInstance(self, condition, instance):
		node = [x for x in instance.conditions if x.child==condition]
		if node:
			instance.removeCondition(node[0])

	def LinkInstance(self, parent, child, position=0):
		return parent.appendInstance( child , position )

	def UnlinkInstance(self, parent, child):
		parent.removeInstance(child)

	def AddInstance(self, parent, name, targetAddress, targetAlias, targetMode, serial, model, position=0):
		def modify(node, target_address, targetAlias, targetMode, serial, model):
			node.targetAddress		= target_address
			node.targetAlias_name  	= targetAlias
			node.targetMode 		= targetMode
			node.serial				= serial
			node.model				= model

		target_address = self.Addresses[targetAddress]
		if parent==None:
			if self.InstanceRoot and self.InstanceRoot.name==name:
				modify(self.InstanceRoot, target_address, targetAlias, targetMode, serial, model )
			else:
				newroot = Instance ( name, target_address, targetAlias, targetMode, serial, model )
				if self.InstanceRoot:
					newroot.appendInstance( self.InstanceRoot , 0 )
				self.InstanceRoot = newroot
				self.session.add(newroot)
			return InstanceHierarchyRoot(self.InstanceRoot)
		elif name in [x.child.name for x in parent.sub_instances]:
			instance_link = [x for x in parent.sub_instances if x.child.name==name][0]
			modify( instance_link.child, target_address, targetAlias, targetMode, serial, model )
			return instance_link
		else:
			return parent.appendInstance( Instance ( name, target_address, targetAlias, targetMode, serial, model ) , position )

	def DeleteInstance(self, parent, name):
		if parent==None:
			del self.InstanceRoot
			self.InstanceRoot=None
		elif name in [x.child.name for x in parent.sub_instances]:
			node = [x for x in parent.sub_instances if x.child.name==name][0]
			parent.removeInstance(node)


def move(collection, node, up=True):
	def moveup(pos, n):
		before = collection[pos-1].previous
		after = n.next
		collection[pos-1].next = after
		collection[pos-1].previous	= n
		n.next = collection[pos-1]
		n.previous = before
		if before: before.next = n
		if after: after.previous = collection[pos-1]
		collection.insert(pos,collection.pop(pos-1))
	position = collection.index(node)
	if up and position > 0:
		moveup(position, node)
	elif not up and position<len(collection)-1:
		moveup(position+1, collection[position+1])


def setup(url):
	print ("\n\n\n----------------------------")
	print ("Creating New Instance")
	print ("----------------------------")

	engine = sqlalchemy.create_engine(url)
	meta = sqlalchemy.MetaData()
	CreateDB(meta)
	meta.create_all(bind = engine)
	engine.echo = True
	MapTables(meta.tables)

	system = Address('System$', 'SQLite', url)
	aliasmap = {'addr':'Address',
				'cond_addr':'Address',
				'resparm':'ResourceParam',
				'cond':'Condition',
				'modl':'Model',
				'modlgrp':'ModelGroup',
				'prnt_modlgrp':'ModelGroup',
				'inst':'Instance',
				'prnt_inst':'Instance',
				'als':'Alias',
				'als1':'Alias',
				'als2':'Alias',
				'lnk':'Link',
				'cim':'ConditionInstanceMap',
				'ins_tree':'InstanceHierarchy',
				'trnlib':'TranLibrary',
				'trnf':'TranFunction',
				'trna':'TranArg'}
	for als, tbl in aliasmap.items():
		system.aliases[als] = Alias(address=system, name=als, resource_name=tbl )
	for res in ['TranArg', 'Condition', 'Model', 'ModelGroup', 'ConditionInstanceMap', 'Instance', 'InstanceHierarchy']:
		system.resourceparams[res] = ResourceParam(address=system, name=res, autoIncField='id')
	for link in [('addr','name','als','address_name'),
				 ('addr','name','als1','address_name'),
				 ('addr','name','als2','address_name'),
				 ('addr','name','resparm','address_name'),
				 ('addr','name','lnk','address_name'),
				 ('cond_addr','name','cond','address_name'),
				 ('addr','name','inst','targetAddress_name'),
				 ('addr','name','modlgrp','address_name'),
				 ('addr','name','prnt_modlgrp','address_name'),
				 ('inst','id','cim','instance'),
				 ('cond','id','cim','condition'),
				 ('inst','id','ins_tree','child'),
				 ('prnt_inst','id','ins_tree','parent'),
				 ('modlgrp','id','modl','group_id'),
				 ('modlgrp','parent','prnt_modlgrp','id'),
				 ('als1','alias','lnk','alias1'),
				 ('als2','alias','lnk','alias2'),
				 ('trnlib','id','trnf','lib'),
				 ('trnf','id','trna','function')]:
		system.links[link] = Link(address=system, alias1_name=link[0], field1=link[1], alias2_name=link[2], field2=link[3], cost=1, jointype=0)

	#System models
	AddressModel = \
'''Address(name)
{
	Str name 						= addr.name,
	Str address_type 				= addr.address_type,
	Str external_address 			= addr.external_address,
	Str db_schema					= addr.db_schema,
	Table associations.RefTo(name) 	= Association[name],
	Table aliases.RefTo(name) 		= Alias[name],
	Table groups.RefTo(name) 		= ModelGroup[name]
}'''
	AliasModel = \
'''Alias(address_name)
{	
	Str name = als.name,
	Str address_name = als.address_name,
	Str resource_name = als.resource_name
}'''
	LinkModel = \
'''Association(address_name)
{
	Str address_name = lnk.address_name,
	Str alias1_name  = lnk.alias1_name,
	Str field1		 = lnk.field1,
	Str alias2_name	 = lnk.alias2_name,
	Str field2	 	 = lnk.field2,
	Int cost 		 = lnk.cost,
	Int jointype	 = lnk.jointype
}'''
	ModelGroupModel = \
'''ModelGroup(address_name)<sequence=id>
{	
	Int id	 		 = Sequence$,
	Int parent_id 	 = NULL$,
	Str name 		 = mdlgrp.name,
	Str address_name = mdlgrp.address_name,
	Str description  = mdlgrp.description	
}'''

	system.modelGroupRoot.models['Alias'] = Model( system.modelGroupRoot, 'Alias', False, AliasModel)
	system.modelGroupRoot.models['Association'] = Model( system.modelGroupRoot, 'Association', False, LinkModel)
	system.modelGroupRoot.models['ModelGroup'] = Model( system.modelGroupRoot, 'ModelGroup', False, ModelGroupModel)
	system.modelGroupRoot.models['Address'] = Model( system.modelGroupRoot, 'Address', False, AddressModel)

	sm = sqlalchemy.orm.sessionmaker( bind=engine )
	session = sm()
	session.add(system)
	session.commit()
	session.close()

if __name__=='__main__':
	#import cProfile
	from .GE_Environment import Environment

	if 'env' not in locals():
		data = {}
		env  = Environment()
		env.SetRunDate('2010/02/22')
		Functions   = ModelFunctionCatalog(env)
		InstanceFns = InstanceFunctionCatalog(env)
		cx 	= Context(env, Functions, InstanceFns)

	#cProfile.run("cx.LoadSystem( 'sqlite:////MarketRisk.db' )", 'loader.prof')

	#cx.LoadSystem( 'sqlite:////MarketRisk.db' )
	#cx.InstanceRoot.sub_instances[2].child.Execute([])
	cx.LoadSystem( 'sqlite:////phil.db' )
	cx.InstanceRoot.Execute([])

	cx.Save()
##	cx.Undo(False)
