import types
import operator
import collections
import sqlalchemy.util

exprOperators = {
	"+": operator.add,
	"-": operator.sub,
	"*": operator.mul,
	"/": operator.truediv,
	"^": operator.pow
	}

typeMap = {
	str: 'String',
	int: 'Int',
	float: 'Real'
}


def evaluateStack(s, fields, environment, objects = None, vars = None):
	
	#this should never break
	op, param = s.pop()
	
	if op == 'Unary':
		return - evaluateStack(s, fields, environment, objects, vars)
	elif op=='NumericOp':
		op2 = evaluateStack( s, fields, environment, objects, vars )
		op1 = evaluateStack( s, fields, environment, objects, vars )
		return exprOperators[param]( op1, op2 )
	elif op == 'StringOp':
		op2 = evaluateStack( s, fields, environment, objects, vars )
		op1 = evaluateStack( s, fields, environment, objects, vars )
		return op1 + op2
	elif op == 'Func' :
		fn, num = param
		paramlist = collections.deque()
		for i in range(num):
			paramlist.appendleft( evaluateStack( s, fields, environment, objects, vars ) )
		return fn ( * tuple ( paramlist ) )
	elif op in ['Field','Ident']:		
		return ( fields [ param ] if environment else ( op, param ) ) if not vars or param not in vars else vars [ param ]
	elif op == 'Object':
		obj, num = param
		paramlist = collections.deque()
		for i in range(num):
			paramlist.appendleft( evaluateStack( s, fields, environment, objects, vars ) )
		if objects:
			objdata = objects[obj]
			return objdata.get( tuple ( paramlist ), DataCollection ([], None, objdata) )
		else:
			return paramlist
	elif op == 'Keyword':
		# note - the keyword could be a reference to ANY environmentally defined object
		return getattr(environment, param)
	else:
		return param


class stack(list):
	def __init__(self, *param):
		self.back=0
		list.__init__(self, *param)

	def pop(self):
		self.back = self.back - 1 if -self.back<len(self) else -1
		return list.__getitem__(self, self.back)


class DataCollection(list):
	def __init__(self, data, key, parent):
		list.__init__(self, data)
		self.key 	= key
		self.parent = parent


# class for holding fields - TODO - replace with pandas
class DataLine(list):	
	def __init__(self, data, parent):	
		list.__init__(self, data)
		self.parent = parent
			
	def __getitem__(self,i):
		return list.__getitem__(self, self.parent.DataMap[i] if type(i)==types.StringType else i)
		
	# manipulation of data	
	def get(self, i, default=None):
		return list.__getitem__(self, self.parent.DataMap[i]) if i in self.parent.DataMap else default

	def getRecord(self):
		return [(fieldname, item) for fieldname, item in zip(self.parent.DataMap, self) if not isinstance(item, list)]
	
	def operate(self, op, value, fields):
		return DataLine([op(item,value) if field in fields else item for field,item in zip(self.parent.DataMap, self)], self.parent)


class DataBlock(sqlalchemy.util.OrderedDict):
	# needs to know about the key linkage between child and parent (ie child.field1=parent.field2 etc.)
	def __init__(self, schema, datatype, primaryseq=[], sequence=[]):
		sqlalchemy.util.OrderedDict.__init__(self)
		keys, elem  				= schema
		self.keytype, self.elemtype = datatype
		self.DataMap 				= sqlalchemy.util.OrderedDict([(k,v) for v,k in enumerate(elem)])
		self.KeyMap 				= sqlalchemy.util.OrderedDict([(k,v) for v,k in enumerate(keys)])
		self.Linkage 				= {}
		self.primaryseqfield		= primaryseq
		self.sequences 				= sequence

	def copy(self):
		return DataBlock( ( self.KeyMap.keys(), self.DataMap.keys() ), (self.keytype, self.elemtype), self.primaryseqfield, self.sequences )
	
	def __iter__(self):
		def merge(values):
			for value in values:
				for element in value:
					yield element
							
		return merge( self.values() )
	
	def GetTypeName(self):
		return '%s(%s)' % ( ','.join( self.KeyMap.keys() ), ','.join( self.DataMap.keys() ) )
							
	def Add(self, key, item):
		sqlalchemy.util.OrderedDict.setdefault(self, key, DataCollection([], key, self)).append(DataLine(item, self))
		
	def LinkKeys(self, datablockfield, listofjoins):
		self.Linkage[datablockfield] = listofjoins
		
class RowElement:
	'''RowElement contains some useful methods to deal with individual attributes of a model.'''
	
	def __init__(self, name, alias, dtype):
		self.dataType 		= dtype
		self.name 			= name
		self.alias 			= alias
		self.exprStack 		= []
		self.fmtStack  		= []
		self.misc			= {}
		self.LinkedTo		= None
		self.QuickFormat 	= None
		self.QuickExpr   	= None
		self.fields  		= set()
		self.idents  		= set()
		self.objs    		= set()

	def Copy(self):
		copy 			= RowElement(self.name, self.alias, self.dataType)
		copy.exprStack	= self.exprStack[:]
		copy.fmtStack	= self.fmtStack[:]
		copy.fields 	= self.fields.copy()
		copy.idents		= self.idents.copy()
		copy.objs 		= self.objs.copy()
		copy.LinkedTo	= self.LinkedTo
		return copy
	
	def parseWith(self, parser):
		self.exprStack, self.fmtStack = parser.Parse( self.fntext )
		self.updateExprStack()
		
	def updateExprStack(self):
		self.fields = set([x[1] for x in self.exprStack if x[0]=='Field'])
		self.idents = set([x[1] for x in self.exprStack if x[0]=='Ident'])
		self.objs   = set([(x[1][0]) for x in self.exprStack if x[0]=='Object'])

	def getFormattedVal (self, value, fields, environment):
		# need some special code here to handle objects.
		if self.fmtStack and not self.dataType.startswith('table'):
			self.QuickFormat[0] = (typeMap [type(value)], value)
			return evaluateStack(self.QuickFormat, fields, environment)
		elif self.dataType.startswith('table'):
			self.misc[value.parent.GetTypeName()] = value.parent
		return environment.Valid
			
	def renumberType(self, fieldmeta, identmeta):
		try:
			self.QuickExpr = stack([(('Field', fieldmeta.index( '$'.join(x[1][:2]))) if x[0]=='Field' else x)
									for x in self.exprStack])
			self.QuickFormat = stack([None]+[
				(('Ident', identmeta.index(x[1])) if x[0]=='Ident' else x)
				for x in self.fmtStack if not self.dataType.startswith('table')])
		except:
			print(fieldmeta, identmeta, self.exprStack, self.fmtStack)
			raise
			
	def getVal(self, fields, environment, objects=None, vars=None):
		return evaluateStack(self.QuickExpr, fields, environment, objects, vars)


if __name__=='__main__':
	if 1:
		from .GE_Parsers import TendrilExpressionParser, TendrilSessionConditionParser, TendrilObjectParser
		from .GE_DataFunctions import GlobalFunctionCatalog, InstanceFunctionCatalog
		from .GE_Environment import Environment
		from .GE_DBInstance import Context
		
		if 'env' not in locals():		
			env 		= Environment()
			env.Rundate = '2007/06/18'
			Functions   = GlobalFunctionCatalog(env)
			InstanceFns = InstanceFunctionCatalog(env)
			cx = Context(env, Functions, InstanceFns)
			cx.LoadSystem( 'tendril.db' )
			
		expr	=	TendrilExpressionParser ( functionLibrary = Functions, parseCondition=True )
		
		RowElementObj  = RowElement( 'test', "Maturity_Date=432+4*(2+1)^3" )
		RowElementObj.parseWith( expr )
		RowElementObj.renumberType([],[])
		RowElementObj.getVal([],None)
		RowElementObj.getFormattedVal(523, [], None)
		test=RowElementObj.getText()