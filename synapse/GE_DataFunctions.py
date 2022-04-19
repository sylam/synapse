import numpy
import bisect
import operator
import functools
import scipy.interpolate

from .GE_RowElement import DataBlock, DataCollection

class VirtualSurface:
	def __init__(self, points):
		self.allpoints = sorted([[x if x else 0.0 for x in y] for y in points])
							   
	def _getVal(self, points, coords):
		i = coords[0]
		l = bisect.bisect([x[0] for x in points],i)
		if l>0:
			if points[l-1][0]==i:
				#hit
				if len(coords)>1:
					return self._getVal([x[1:] for x in points if x[0]==i], coords[1:])
				else:
					return points[l-1][-1]
			elif l==len(points):
				#cap at upper
				if len(coords)>1:
					return self._getVal([x[1:] for x in points if x[0]==points[-1][0]], coords[1:])
				else:
					return points[-1][-1]
			elif points[l-1][0]<i and points[l][0]>i:
				#interp
				if len(coords)>1:
					point1 = self._getVal([x[1:] for x in points if x[0]==points[l-1][0]], coords[1:])
					point2 = self._getVal([x[1:] for x in points if x[0]==points[l][0]], coords[1:])
					return ( point2 * ( i - points[l-1][0] ) + point1 * ( points[l][0] - i ) ) / ( points[l][0] - points[l-1][0] )
				else:
					return ( points[l-1][-1] * ( i - points[l-1][0] ) + points[l][-1] * ( points[l][0] - i ) ) / ( points[l][0] - points[l-1][0] )
		else:
			#floor at lower
			if len(coords)>1:
				return self._getVal([x[1:] for x in points if x[0]==points[0][0]], coords[1:])
			else:
				return points[0][-1]
				
	def getVal(self, coords):
		return self._getVal(self.allpoints, coords)

class InstanceFunctionCatalog:
	def __init__(self, environment):
		self.environment = environment
			
	#RowObject Formatting Functions
	def Contract(self, delimiter, *RowObjects):
		'''
		Contract(delimiter, *RowObjects)
		The opposite of an extend - joins individual
		rowobjects with the given delimiter
		'''
		return delimiter.join( functools.reduce ( operator.add, RowObjects ) )

	def GetFirstAttr(self, RowObject, field):
		'''
		GetAttr(Model, Attribute)
		Returns the Attribute for the given model. If there are several rows in
		the model, only the first row's attribute is returned.
		'''
		return RowObject[0][field] if RowObject else None
	
	def Wangle(self, RowObject):
		'''
		Wangle(Model):
		No Idea - try it and see.
		'''
		if RowObject:
			return ' '.join( RowObject[0] )
		else:
			return 'End'
	
	def AsText(self, RowObject):
		'''
		AsText(RowObject) - simply returns the rowobject
		as a text field (useful for debugging)
		'''
		return str(RowObject)

	def FirstRowObjectByList(self, RowObject, Attribute, ReturnAttribute, Value ):
		'''
		FilterRowObjectByList ( RowObject, Attribute, ReturnAttribute , Value )
		Filters a RowObject - Basically if RowObject[Attribute] in Value,
		return the ReturnAttribute.
		'''
		data = [x for x in RowObject if x[Attribute] in Value]		
		return data[0][ReturnAttribute] if data else ''		
		
	def ExtVer(self, *RowObject):
		'''
		ExtVer(*RowObject)
		Extends the RowObject Vertically in the output
		'''
		return ('Vertical', RowObject[0] if len(RowObject)==1 else list(RowObject))
	
	def ExtHor(self, *RowObject):
		'''
		ExtHor(*RowObject)
		Extends the RowObject Horizontally in the output
		'''
		return ('Horizontal', RowObject[0] if len(RowObject)==1 else list(RowObject))

class ModelFunctionCatalog(InstanceFunctionCatalog):
	def __init__(self, environment):
		InstanceFunctionCatalog.__init__(self, environment)

	def ValidateList(self, field, errorlevel, *args):
		'''
		ValidateList(errorlevel, *args) - checks if the field is in the list provided.
		If it is, return the value. If not, look at the errorlevel and either
		log a warning (errorlevel=Warning$) or skip the row (errorlevel=Error$)
		'''
		if field not in args:
			return self.environment.ProcessErrorLevel ( errorlevel, field, 'ValidateList - (%s)' % str(args) )
		else:
			return self.environment.Valid
		
	def Call(self, obj, method, *args):
		'''
		Call(obj, method, *args) - calls the given method on object obj with *args i.e. obj.method(*args)
		Necessary as only straightforward thread-safe C functions are supported
		'''
		return getattr(obj, method)(*args)

	def Apply(self, scentype, scenvalue, scenvar, scencurve, yieldcurve):
		'''
		Apply(scentype, scenvalue, scenvar, scencurve, yieldcurve) - performs scentype modification to yieldcurve
		based on scenvalue, scenvar and scencurve.
		'''
		
		if scentype=='parallel shift' and yieldcurve.key[0]==scenvar:
			#virtualmodel = self.environment.GetVirtualModel(yieldcurve.parent)
			#if args[0].key not in virtualmodel:
			#ignoreing scencurve at the moment
			return DataCollection( [x.operate(operator.iadd, scenvalue, ['Value']) for x in yieldcurve], yieldcurve.key, yieldcurve.parent )
		else:
			return yieldcurve
		
	def SplitIndex(self, arg, spt, index):
		'''
		SplitIndex(arg, split, index):	Returns the split index
		'''
		try:
			return arg.split(spt)[index]
		except:
			return arg.split(spt)[-1]
	
	def Reduce(self, op, *args):
		'''
		Reduce(op, *models) - applies the given operator to the list of models returning a single model
		Note: All the models have to be of the same type and the operator defined on the type.
		'''		
		virtualmodel = self.environment.GetVirtualModel(args[0].parent)
		if args[0].key not in virtualmodel:
			for v1,v2 in zip(*self.AddTimes(args[0], args[1])):
				virtualmodel.Add(args[0].key, [None, v1, v2])
		return virtualmodel[args[0].key]	

	def AddTimes(self, v1, v2):
		t1= [ yc['Time'] for yc in v1 ]
		y1= [ yc['Value'] for yc in v1 ]
		tck1 = scipy.interpolate.interp1d(numpy.array(t1),numpy.array(y1))
		if v2:
			t2			= [ yc['Time'] for yc in v2 ]
			y2			= [ yc['Value'] for yc in v2 ]
			tck2 		= scipy.interpolate.interp1d(numpy.array(t2),numpy.array(y2))
			allpoints 	= sorted(set(t1).union(t2))
			results 	= [ operator.add ( tck1(t) , tck2(t) ) for t in allpoints ]
			return ( allpoints, results )
		else:
			return (t1, y1)

	def VirtualModel(self, Keys, KeyTypes, Elements, ElementTypes,  primaryseq , sequence):
		'''
		VirtualModel(Keys, KeyTypes, Elements, ElementTypes,  primaryseq , sequence) -
		Creates a Virtual Model (i.e. a model that looks like a real model and acts like one,
		but you define in scope and disappears afterward)
		'''
		block 			= DataBlock( ( Keys, Elements ), (KeyTypes, ElementTypes), primaryseq if primaryseq else [], sequence if sequence else [] )
		return self.environment.GetVirtualModel(block)		

	def Set(self, obj, key, *values):
		'''
		Set(obj, keys, *values) - Manually adds values to obj for the given key (usually used
		when dealing with Virtual Models)
		'''
		for value in values:
			obj.Add(key, list(value))
		return obj[key]
	
	def Merge(self, obj, opfield, aggfields, interpfields, *fields):
		'''
		Merge(models, opfield, aggfields, interpfields, *fields) - takes function at opfield
		and applies it to the list of models (specified by fields) to create a new dataset
		( assumes that the first field specified in fields is the type of object to perform
		the merge on ). The merge itself is done by using aggfields and interpfields.
		Returns the merged object but aggregated on obj's set of aggregation keys.
		'''
		if obj:
			virtualmodel = self.VirtualModel( obj.parent.KeyMap.keys(),
											  obj.parent.keytype,
											  obj[0].get(fields[0]).parent.DataMap.keys(),
											  obj[0].get(fields[0]).parent.elemtype,
											  obj.parent.primaryseqfield,
											  obj.parent.sequences )			
			if obj.key not in virtualmodel:
				op 		= getattr(self.environment, set ( self.StripReduce(obj,opfield) ).pop())
				vectors	= [ x for x in self.StripReduce(obj, *fields) if x ]
				mergeop	= self.Curry(op, aggfields, interpfields)
				for line in mergeop(vectors):
					virtualmodel.Add(obj.key, line)
			return virtualmodel[obj.key]
		return obj

	def Curry2(self, op, aggfields, interp):
		'''
		Curry2(op, aggfields, interp) - returns a function that aggregates models by aggfields and
		interpolates missing numeric data by iterpolating on the fields specified by interp
		'''
		def functionop(objects):			
			if len(objects)>1:
				domainindex				= aggfields.index(interp[0])
				res_agg					= self.Strip(objects[0], *aggfields)
				res_domain, res_range	= zip(*self.Strip(objects[0], *interp))
				res_tck 				= scipy.interpolate.interp1d(numpy.array(res_domain),numpy.array(res_range))
				
				for obj in objects[1:]:				
					t2 					= self.Strip(obj, *aggfields)
					domain2, range2		= zip(*self.Strip(obj, *interp))
					tck2 				= scipy.interpolate.interp1d(numpy.array(domain2),numpy.array(range2))
					
					res_agg					= sorted(set([tuple(x) for x in res_agg]).union([tuple(y) for y in t2]))
					res_domain, res_range	= zip( * [ ( t[domainindex], op ( res_tck(t[domainindex]), tck2(t[domainindex]) ) ) for t in res_agg ] )
					res_tck 				= scipy.interpolate.interp1d(numpy.array(res_domain),numpy.array(res_range))
					
				return [ t + tuple([float(res_tck(t[domainindex]))]) for t in res_agg ]
			else:	
				return objects[0]
		
		return functionop
	
	def Curry(self, op, aggfields, interp):
		'''
		Curry(op, aggfields, interp) - returns a function that aggregates models by aggfields and
		interpolates missing numeric data by iterpolating on the fields specified by interp
		'''
		def functionop(objects):			
			if len(objects)>1:
				domainindex	    = aggfields.index(interp[0])
				res_agg		    = self.Strip(objects[0], *aggfields)
				res_surface     = VirtualSurface ( self.Strip(objects[0], *interp) )
				
				for obj in objects[1:]:				
					t2 					= self.Strip(obj, *aggfields)
					temp_surface2		= VirtualSurface ( self.Strip(obj, *interp) )
					
					res_agg					= sorted(set([tuple(x) for x in res_agg]).union([tuple(y) for y in t2]))
					res_surface             = VirtualSurface ( [ [t [domainindex], op ( res_surface.getVal ( [ t [domainindex] ] ), temp_surface2.getVal( [ t[domainindex] ] ) ) ] for t in res_agg ] )
					
				return [ t + tuple([res_surface.getVal ( [ t[domainindex] ] )]) for t in res_agg ]
			else:	
				return objects[0]
		
		return functionop

	def BinOp(self, op, field, val):
		'''
		BinOp(op, field, val) - simple (really simple) function that returns a function that performs
		binary op on field and val
		'''
		return lambda x:op(x.get(field),val)
	
	def Sort(self, obj, *keys):
		'''
		Sort(obj, *keys) - sorts obj by the given keys
		'''
		return sorted( obj, key=lambda x:tuple([x.get(f) for f in keys]) )

	def Strip(self, obj, *fields):
		'''
		Strip(obj, *fields) - returns only the specified fields from obj
		'''
		return [[x.get(f) for f in fields] for x in obj]
	
	def StripReduce(self, obj, *fields):
		'''
		StripReduce(obj, *fields) - returns only the specified fields from obj
		but concatenates the results in 1 list (as opposed to Strip)
		'''
		return functools.reduce(operator.concat, [[x.get(f) for f in fields] for x in obj], [])
	
	def Filter(self, fn, obj):
		'''
		Filter(fn, obj) - returns only the rows specified after applying fn
		fn is usually a BinOp or some other Op . . .
		'''
		return [x for x in obj if fn(x)]
	
	def List(self, *args):
		'''
		List(*args): creates (and returns) a list type made up of the given arguments
		'''
		return args

	def For(self, row, loopindex):
		'''
		Not working . . .
		'''
		result = []
		for self.environment.LoopCounter in loopindex:
			result.append(row)

	def Get(self, obj, *attrib):
		'''
		Get(obj,*attrib) - returns the corresponding attribute in obj - used mainly for properties
		Use Call(obj,method,*args) if methods need to be called
		'''
		x = getattr(obj, attrib[0])
		for a in attrib[1:]: x=getattr(x, a)
		return x
	
	def Decode(self, *args):
		'''Decode(arg1, arg2, ... ) - Oracle nested if then else clone'''
		for i in range ( (len(args)/2)-1 ):
			if args[2*i+1]==args[0]:
				return args[2*i+2]
		return args[-1]
	
	def FWFormat(self, field, fixedwidth, justification, fieldtype):
		'''
		FWFormat(data, fixedwidth, justification, fieldtype) - formats data as fixed width,
		with either left or right justification and data type given by fieldtype - padding is
		added according to fieldtype.
		'''
		formatstring = '%%%s0%d%s' % (justification, fixedwidth, fieldtype)
		return formatstring % field
	
	def Maximum(self, *args):
		'''Maximum (*args) - returns the maximum of a sequence of arguments'''
		return max(args)

	def Minimum(self, *args):
		'''Minimum (*args) - returns the minimum of a sequence of arguments'''
		return min(args)

	def Format(self, *args):
		'''Format(*args) - formatted text specified by args'''
		return args[0] % (args[1:])

	def Replace(self, orig, old, new):
		'''Replace(orig, new) - find and replace'''
		return orig.replace(old,new)

	def Round(self, arg, numdigits=2):
		'''Round(num, numdigits=2) - rounds off to numdigits places (default 2)'''
		return round(arg, numdigits)