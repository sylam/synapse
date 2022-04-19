import types
import collections
import sqlalchemy.sql

from pyparsing import *
from .GE_Exception import *
from .GE_RowElement import evaluateStack, typeMap


# each parser defines it's own tokens - keep it that way - as wasteful as it seems . . .

def flatten(sequence):
    for item in sequence:
        if isinstance(item, list):
            for subitem in flatten(item):
                yield subitem
        else:
            yield item


class Block:
    '''Assumes blocks are always padded (rectangular)'''

    def __init__(self, data, numrows=0, numcols=0):
        self.rows = numrows
        self.cols = numcols
        self.data = data

    def __iter__(self):
        return self.data.__iter__()

    def AddBelow(self, otherblock):
        delta = otherblock.cols - self.cols
        if delta > 0:
            for row in self.data:
                row.extend([''] * delta)
        for row in otherblock.data:
            self.data.append(row + [''] * -delta)

        self.rows = len(self.data)
        self.cols = max(otherblock.cols, self.cols)

    def AddRight(self, otherblock):
        delta = otherblock.rows - self.rows
        if delta > 0:
            for row in range(delta):
                self.data.append([''] * self.cols)
        for index, row in enumerate(self.data):
            row.extend(otherblock.data[index] if index < otherblock.rows else [''] * otherblock.cols)

        self.rows = max(self.rows, otherblock.rows)
        self.cols = otherblock.cols + self.cols


def ExtendFormat(ling, commandhist, index):
    if type(ling) == types.TupleType:
        return ExtendFormat(ling[1], commandhist + [ling[0]], index)
    elif isinstance(ling, list) or isinstance(ling, collections.deque):
        res = Block([])
        for item in ling:
            val = ExtendFormat(item, commandhist, index + 1)
            if commandhist[index] == 'Vertical':
                res.AddBelow(val)
            elif commandhist[index] == 'Horizontal':
                res.AddRight(val)
        return res
    else:
        return Block([[ling]], numrows=1, numcols=1)


class TendrilExpressionParser:

    def __init__(self, functionLibrary=None, parseCondition=False):
        self.exprStack = []
        self.condStack = []
        self.attribs = None
        self.Functions = functionLibrary
        self.parseCondition = parseCondition
        self.parser = self.Grammer()
        self.modulecache = {}

    def Parse(self, input):
        self.exprStack, self.condStack = [], []
        self.parser.parseString(input)
        return self.exprStack, self.condStack

    def Grammer(self):

        def pushKeyword(strg, loc, toks):
            self.exprStack.append(('Keyword', toks[0]))
            return toks[0] + '$'

        def pushFunction(strg, loc, toks):
            # need to extend to use ctypes
            fullname = toks[:-1]
            try:
                if len(fullname) > 1:
                    fnkey = tuple(fullname)
                    if fnkey in self.modulecache:
                        fn = self.modulecache[fnkey]
                    else:
                        fn = getattr((__import__('.'.join(fullname[:-1]), [fullname[-1]], 0)), fullname[-1])
                        self.modulecache[fnkey] = fn
                else:
                    fn = getattr(self.Functions, toks[0])
            except:
                self.exprStack = []
                self.condStack = []
                raise FunctionNotFoundException(toks[0])

            func_data = [fn, toks[-1].asList().count(',') + 1]
            self.exprStack.append(('Func', func_data))

        def pushField(strg, loc, toks):
            if toks[1] == '.':
                field = (toks[0].lower(), toks[2], None)
            else:
                field = (toks[1].lower(), toks[3], toks[0].lower())
            self.exprStack.append(('Field', field))

        def pushIdent(strg, loc, toks):
            self.exprStack.append(('Ident', toks[0]))

        def pushObject(strg, loc, toks):
            obj_data = [tuple(toks[:-1]), toks[-1].asList().count(',') + 1]
            self.exprStack.append(('Object', obj_data))

        # obj_data[-1].update( [x[1] for x in evaluateStack(self.exprStack[:], [], None) if x[0]=='Ident'] )

        def pushAttrib(strg, loc, toks):
            # not happy here - feels icky
            if self.exprStack[1:]:
                self.condStack = self.exprStack[1:]
                if toks[0].lower() != 'table':
                    self.condStack[-1][-1][-1] += 1
            self.exprStack = []
            if toks[-2].lower() == 'as':
                self.attribs = (toks[0].lower(), toks[-1])
            else:
                self.attribs = (toks[0].lower() + (toks[3].lower() if toks[0].lower() == 'table' else ''), None)

        def pushSringOp(strg, loc, toks):
            self.exprStack.append(('StringOp', toks[0]))

        def pushNumericOp(strg, loc, toks):
            self.exprStack.append(('NumericOp', toks[0]))

        def pushInt(strg, loc, toks):
            self.exprStack.append(('Int', int(toks[0])))

        def pushReal(strg, loc, toks):
            self.exprStack.append(('Real', float(toks[0])))

        def pushString(strg, loc, toks):
            self.exprStack.append(('Str', str(toks[0])))

        def pushUMinus(strg, loc, toks):
            if toks and toks[0] == '-':
                self.exprStack.append(('Unary', toks[0]))

        plus = Literal("+")
        minus = Literal("-")
        mult = Literal("*")
        div = Literal("/")
        concat = Literal("||")

        table_ = Keyword("Table", caseless=True)
        link_ = Keyword("Link", caseless=True)
        refto_ = Keyword("RefTo", caseless=True)
        refby_ = Keyword("RefBy", caseless=True)
        as_ = Keyword("as", caseless=True)

        lpar = Literal("(").suppress()
        rpar = Literal(")").suppress()
        lsqpar = Literal("[").suppress()
        rsqpar = Literal("]").suppress()

        qString = (QuotedString("'", '\\', "''") | QuotedString('"', '\\', '""')).setName('string')
        label = (QuotedString("'", '\\', "''")).setName('label')

        addop = plus | minus
        multop = mult | div
        expop = Literal("^")

        groupfn = oneOf("avg max min count sum", caseless=True).setName('group')
        types = oneOf("Int Real Str", caseless=True).setName('datatype')

        e = CaselessLiteral("E")
        point = Literal(".")
        comma = Literal(",")
        dollar = Literal("$")
        ident = Word(alphas, alphas + nums + "_")
        namedId = (delimitedList(ident, delim='.', combine=False)).setName('namedId')
        attr = (ident + ~(lpar | dollar | lsqpar)).setName('attribute')

        sql_col = (ident + point + ident).setName('sql_col')
        field = (sql_col | groupfn + lpar + sql_col + rpar).setName('Field')

        integer = (Word("+-" + nums, nums) + ~point).setName('int')
        fnumber = Combine(Word("+-" + nums, nums) + Optional(point + Optional(Word(nums))) + Optional(
            e + Word("+-" + nums, nums))).setName('float')

        expr = Forward()

        objattr = (namedId + lsqpar + Group(expr + ZeroOrMore(comma + expr)) + rsqpar).setName('objattribute')
        func = namedId + lpar + Group(expr + ZeroOrMore(comma + expr)) + rpar

        atom = (Optional("-") +
                (integer.setParseAction(pushInt) |
                 fnumber.setParseAction(pushReal) |
                 qString.setParseAction(pushString) |
                 (ident + dollar).setParseAction(pushKeyword) |
                 func.setParseAction(pushFunction) |
                 (objattr.setParseAction(pushObject) | field.setParseAction(pushField) | attr.setParseAction(
                     pushIdent)) |
                 (lpar + expr.suppress() + rpar))).setParseAction(pushUMinus)

        factor = Forward()
        factor << (atom + ZeroOrMore((expop + factor).setParseAction(pushNumericOp)))

        term = (factor + ZeroOrMore((multop + factor).setParseAction(pushNumericOp))).setName('term')
        expr << (term + ZeroOrMore(
            (addop + term).setParseAction(pushNumericOp) | (concat + term).setParseAction(pushSringOp)))

        attrib = ((table_ + attr + point + (link_ | refto_ | refby_) + lpar + delimitedList(attr) + rpar) |
                  (types + attr + Optional(point + func.setParseAction(pushFunction)) + Optional(
                      as_ + label))).setParseAction(pushAttrib)
        equals = Literal("=")
        condition = attrib + equals + expr

        return (condition if self.parseCondition else expr)


class TendrilObjectParser:
    def __init__(self, functionLibrary=None):
        self.objects = []
        self.variables = {}
        self.attribs = []
        self.Functions = functionLibrary
        self.assignparser = TendrilExpressionParser(functionLibrary, parseCondition=True)
        self.varparser = TendrilExpressionParser(functionLibrary, parseCondition=False)
        self.objparser = self.Grammer()

    def ParseObject(self, input):
        self.objects = None
        self.attribs = []
        self.variables = {}
        self.objparser.parseString(input)
        return self.objects

    def Grammer(self):

        def pushAttribute(strg, loc, toks):
            self.attribs.append(
                self.assignparser.attribs + (toks[1], self.assignparser.exprStack, self.assignparser.condStack))
            self.assignparser.exprStack = []
            self.assignparser.condStack = []

        def pushVariable(strg, loc, toks):
            self.variables[toks[1]] = self.varparser.exprStack[:]
            self.varparser.exprStack = []

        def pushRowObject(strg, loc, toks):
            switches = {}
            if hasattr(toks[2], 'asDict'):
                data = toks[2].asList()
                switches = {}
                for key, val in [(data[2 * x], data[2 * x + 1]) for x in range(len(data) // 2)]:
                    switches.setdefault(key, []).append(val)
                for key, val in switches.items():
                    if len(val) == 1: switches[key] = val[0]

            # handle recursive rowobjects
            keylist = toks[1].asList()
            index = keylist.index(';') if ';' in keylist else len(keylist)
            keys, rkeys = keylist[:index], keylist[index + 1:]

            self.objects = (toks[0], keys, rkeys, switches, self.variables, self.attribs)

        lpar = Literal("(").suppress()
        rpar = Literal(")").suppress()

        lapar = Literal("<").suppress()
        rapar = Literal(">").suppress()

        lcurpar = Literal("{").suppress()
        rcurpar = Literal("}").suppress()

        lsqpar = Literal("[").suppress()
        rsqpar = Literal("]").suppress()

        equals = Literal("=").suppress()
        comma = Literal(",").suppress()

        qString = (QuotedString("'", '\\') | QuotedString('"', '\\')).setName('string')
        ident = Word(alphas, alphas + nums + "_")
        attr = (ident + ~(lpar | lsqpar)).setName('attribute')

        select_ = Keyword("select", caseless=True)
        primary_ = Keyword("primary", caseless=True)
        sequence_ = Keyword("sequence", caseless=True)
        var_ = Keyword("var", caseless=True)

        variable = var_ + attr + equals + self.varparser.parser
        argument = self.assignparser.parser
        switch = select_ + equals + ident | primary_ + equals + ident | sequence_ + equals + ident

        assign = (~lsqpar + ident + lpar + Group(delimitedList(attr) + Optional(';' + delimitedList(attr))) + rpar
                  + Optional(Group(lapar + delimitedList(switch) + rapar))
                  + Optional(lsqpar + delimitedList(variable.setParseAction(pushVariable)) + rsqpar)
                  + lcurpar + delimitedList(argument.setParseAction(pushAttribute)) + rcurpar).setParseAction(
            pushRowObject)

        assign.ignore(cppStyleComment)

        return assign


class SynapseConditionParser:
    def __init__(self, environment, schema, functionLibrary=None):
        self.exprStack = []
        self.Functions = functionLibrary
        self.env = environment
        self.schema = schema
        self.conditionparser = self.Grammer()
        self.modulecache = {}
        self.operators = {'>': '__gt__',
                          '<': '__lt__',
                          '<=': '__le__',
                          '>=': '__ge__',
                          '=': '__eq__',
                          '<>': '__ne__',
                          '!=': '__ne__',
                          'like': 'like'}

    def ParseCondition(self, data):
        self.exprStack = []
        self.Having = False
        self.aliases = set()
        whereclause = self.conditionparser.parseString(data)
        return whereclause.pop(), self.aliases, self.Having

    def Grammer(self):

        def pushKeyword(strg, loc, toks):
            self.exprStack.append(('Keyword', toks[0]))
            val = getattr(self.env, toks[0])
            # return "'%s'" % val if type(val)==types.StringType else val
            return "'%s'" % val if type(val) == types.StringType else (val if val else 'NULL')

        def pushField(strg, loc, toks):
            field = (toks[0].lower(), toks[2].lower())
            self.aliases.add(field[0])
            return self.schema.aliastables[field[0]][1].columns[toks[2]]

        def pushFunction(strg, loc, toks):
            fullname = toks[:-1]
            if len(fullname) > 1:
                fnkey = tuple(fullname)
                if fnkey in self.modulecache:
                    fn = self.modulecache[fnkey]
                else:
                    fn = getattr((__import__('.'.join(fullname[:-1]), globals(), locals(), [fullname[-1]], 0)),
                                 fullname[-1])
                    self.modulecache[fnkey] = fn
                    func_data = [fn, len(toks[-1])]
            elif hasattr(self.Functions, toks[0]):
                func_data = [getattr(self.Functions, toks[0]), len(toks[-1])]
            else:
                if toks[0].lower() in ['avg', 'max', 'min', 'count', 'sum']:
                    self.Having = True
                fn = getattr(sqlalchemy.sql.func, toks[0])
                return fn(*tuple(toks[-1]))

            self.exprStack.append(('Func', func_data))
            result = evaluateStack(self.exprStack, [], self.env)
            self.exprStack.append((typeMap.get(type(result), type(result)), result))
            return result

        def pushSringOp(strg, loc, toks):
            self.exprStack.append(('StringOp', toks[0]))

        def pushNumericOp(strg, loc, toks):
            self.exprStack.append(('NumericOp', toks[0]))

        def pushInt(strg, loc, toks):
            self.exprStack.append(('Int', int(toks[0])))
            return int(toks[0])

        def pushReal(strg, loc, toks):
            self.exprStack.append(('Real', float(toks[0])))
            return float(toks[0])

        def pushString(strg, loc, toks):
            self.exprStack.append(('Str', toks[0]))
            return toks[0][1:-1]

        def pushUMinus(strg, loc, toks):
            if toks and type(toks[0]) == str and toks[0] == '-' and self.exprStack and self.exprStack[-1] != (
            'Str', "'-'"):
                self.exprStack.append(('Unary', toks[0]))

        def pushNotOp(strg, loc, toks):
            if toks and type(toks[0]) == str and toks[0] == 'not':
                return ~toks[1]

        def pushBinOp(strg, loc, toks):
            return getattr(toks[0], self.operators[toks[1]])(None if toks[2] == 'NULL' else toks[2])

        def pushList(strg, loc, toks):
            return ~toks[0].in_(toks[2].asList()) if toks[1] == 'not' else toks[0].in_(toks[1].asList())

        def pushBetweenOp(strg, loc, toks):
            return toks[0].between(toks[1], toks[2])

        def pushConjunction(strg, loc, toks):
            return sqlalchemy.and_(*tuple(toks[-1])) if toks[0].lower() == 'and' else sqlalchemy.or_(*tuple(toks[-1]))

        binop = oneOf("= != <> < > >= <= like is", caseless=True).setName('binop')

        e = CaselessLiteral("E")
        plus = Literal("+")
        minus = Literal("-")
        mult = Literal("*")
        div = Literal("/")
        concat = Literal("||")

        addop = plus | minus
        multop = mult | div
        expop = Literal("^")

        lpar = Literal("(").suppress()
        rpar = Literal(")").suppress()
        point = Literal(".")
        dollar = Literal("$")
        comma = Literal(",").suppress()

        and_ = Keyword("and", caseless=True)
        or_ = Keyword("or", caseless=True)
        not_ = Keyword("not", caseless=True)
        in_ = Keyword("in", caseless=True).suppress()
        bet_ = Keyword("between", caseless=True).suppress()

        ident = Word(alphas, alphas + nums + "_")
        namedId = (delimitedList(ident, delim='.', combine=False)).setName('namedId')
        integer = (Word("+-" + nums, nums) + ~point).setName('int')
        fnumber = Combine(Word("+-" + nums, nums) + Optional(point + Optional(Word(nums))) + Optional(
            e + Word("+-" + nums, nums))).setName('float')

        qString = (QuotedString("'", '\\', "''", unquoteResults=False) | QuotedString('"', '\\', '""',
                                                                                      unquoteResults=False)).setName(
            'string')

        field = (ident + point + ident + ~(lpar | point)).setName('field')

        expr = Forward()

        func = namedId + lpar + Group(expr + ZeroOrMore(comma + expr)) + rpar

        atom = (Optional("-") +
                (integer.setParseAction(pushInt) |
                 fnumber.setParseAction(pushReal) |
                 qString.setParseAction(pushString) |
                 (ident + dollar).setParseAction(pushKeyword) |
                 func.setParseAction(pushFunction) |
                 field.setParseAction(pushField) |
                 (lpar + expr.suppress() + rpar))).setParseAction(pushUMinus)

        factor = Forward()
        factor << (atom + ZeroOrMore((expop + factor).setParseAction(pushNumericOp)))

        term = (factor + ZeroOrMore((multop + factor).setParseAction(pushNumericOp))).setName('term')
        expr << (term + ZeroOrMore(
            (addop + term).setParseAction(pushNumericOp) | (concat + term).setParseAction(pushSringOp)))

        whereCondition = (Optional(not_) + (
                    (expr + bet_ + expr + and_.suppress() + expr).setParseAction(pushBetweenOp) | \
                    (expr + binop + expr).setParseAction(pushBinOp))).setParseAction(pushNotOp) | \
                         (expr + Optional(not_) + in_ + lpar + (Group(fnumber + ZeroOrMore(comma + fnumber)) | Group(
                             integer + ZeroOrMore(comma + integer)) | Group(
                             qString + ZeroOrMore(comma + qString))) + rpar).setParseAction(pushList)

        whereExpression = Forward()
        whereExpression << (whereCondition | ((and_ | or_) + lpar + Group(
            whereExpression + ZeroOrMore(comma + whereExpression)) + rpar).setParseAction(pushConjunction))

        return whereExpression


if __name__ == '__main__':
    if 1:
        from .GE_DataFunctions import ModelFunctionCatalog, InstanceFunctionCatalog
        from .GE_Environment import Environment
        # from GE_Context import Context
        from .GE_DBInstance import Context

        ##		from datetime import datetime
        ##		from sqlalchemy import create_engine, MetaData, Table, Column, Sequence, ForeignKey, UniqueConstraint, and_, or_
        ##		from sqlalchemy import Integer, String, Text, Boolean, DateTime
        ##		from sqlalchemy.orm import sessionmaker, scoped_session, create_session, mapper, relation, backref, dynamic_loader, aliased, reconstructor, join
        from sqlalchemy.orm.session import Session
        from sqlalchemy.sql import select, func

        ##		from sqlalchemy.util import OrderedDict
        ##		from sqlalchemy.orm.collections import column_mapped_collection, mapped_collection, attribute_mapped_collection

        if 'env' not in locals():
            env = Environment()
            env.Rundate = '2009/12/08'
            Functions = ModelFunctionCatalog(env)
            InstanceFns = InstanceFunctionCatalog(env)
            cx = Context(env, Functions, InstanceFns)
            # cx.LoadSystem( 'tendril.db' )
            cx.LoadSystem('sqlite:////marketrisk.db')

        expr = TendrilExpressionParser(functionLibrary=Functions, parseCondition=True)
        print(expr.Parse("Str Maturity_Date as 'bob day'=cfw.payday*2 - goodbye[death,'FX_'||cur.insid||'_US,D']"))

        cond = SynapseConditionParser(cx.environment, cx.Addresses['ArenaDev'].schema, functionLibrary=Functions)

        # test    =	cond.ParseCondition("als.alias > substring(Rundate$, '%Y-%m-%d') and roa.fuck like '%smith%'" )
        # test    =	cond.ParseCondition("not max(als.alias) like Replace(Rundate$, '/','-')" )
        # test    =	cond.ParseCondition("and ( or ( max(als.name) between substring(Rundate$, '%Y-%m-%d') and '2', als.name='bob' ), or ( als.address_name='System', addr.name<>'Mount' ) )" )
        test = cond.ParseCondition("or ( ins.exp_day=NULL$, ins.exp_day > Replace(Rundate$,'/','-') )")
        # test    =	cond.ParseCondition("als.alias not in (2,21,3,5)" )
        # test    =	cond.ParseCondition("als.alias > math.log(mx.DateTime.DateFrom(Rundate$))" )
        test = cond.ParseCondition("ANALYZE")

        objtest = '''YieldCurvePoint(ID)<select=distinct>
					[
					var todaysDate = QuantLib.Date(Rundate$,'yyyy/mm/dd')
					]
					{
						Str  ID 	 			= yc.yield_curve_name,
						Int  Time 				= Call(ZARCalendar$, 'adjust', todaysDate + QuantLib.Period(ycpt.date_period_count, ycpt.date_period_count - 1 ) - todaysDate ),
						Real Value 				= ycpt.value
					}'''
        objtest = '''
					ScenarioZeroCurve ( ID, ScenarioID; Child, ScenarioID )<select=distinct>
					{
						Str  ID  				= own_crv.ID,
						Str	 Child				= crv.ID,
						Str  ScenarioID			= scen.ID,
						Str	 Yieldcp			= AsText(YieldCurvePoint[crv.ID]),
						Str  ScenCurvePoint		= AsText(ScenarioCurvePoint[scen_crv.ID]),
						Table curve.RefTo(ID) 	= ScenarioZeroCurve [crv.ID, scen.ID]
					}
					'''
        energyforward = '''EnergyForward(name)<primary=id,primary=name>
				{
				Str id 				as 'ID'						=	Format('FUT_%s_%s_%d',trds.EXCHANGE, GetFirstAttr(Commodities.Stream [ trds.TRADEID, 1],'Curr'), trds.TRADEID),
				Str name 			as 'Name'					=	Format('FUT_%s_%d',trds.EXCHANGE, trds.TRADEID),
				Str currency		as 'Currency'				=	GetFirstAttr( Commodities.Stream [ trds.TRADEID, 2 ], 'Curr'), 
				Str discount_curve 	as 'Discount Curve'			=	Format(':IR_%s_RESULTS', GetFirstAttr( Commodities.Stream [ trds.TRADEID, 2 ], 'Curr') ),
				Str maturity_date 	as 'Maturity Date'			=	trds.MATURITY, 
				Str pricing_date 	as 'Pricing Date'			=	'1900/01/01',
				Real strike_price 	as 'Strike Price'			=	trds.PSTRIKE,
				Real notional 		as 'Notional'				=	1,
				Str run_avg			as 'Running average'		=	GetFirstAttr( Commodities.Stream [ trds.TRADEID, 2 ],'FixRateNext'),
				Str under_curve_idx as 'Underlying Curve Index'	=	Format( '%s_CURVE_INDEX', GetFirstAttr( Commodities.Stream [ trds.TRADEID, 1],'Curr') ), 
				Str hist_curve 		as 'Historical Curve'		=	Format( '%s_%s_HIST_CURVE', GetFirstAttr( Commodities.Stream [ trds.TRADEID, 1], 'Curr') , GetFirstAttr( Commodities.Stream [ trds.TRADEID, 2 ],'Curr') ), 
				Real spot_price 	as 'Spot Price'				=	trds.FORWARD,
				Str flow_delivery 	as 'Flow Delivery'			=	'FALSE'
				Str bus_day_rule 	as 'Business Day Rule'		=	'None (Standard)',
				Str sett_day_rule 	as 'Settlement Day Rule'	=	'None (Standard)',
				Str sett_type 		as 'Settlement Type'		=	'Cash Settlement',
				Str sett_proc 		as 'Settlement Procedure'	=	'@commodities settlement',
				Str term_len 		as 'Term Length'			=	'@maturity relative days',
				Str counterpty 		as 'Counterparty'			=	trds.CPARTY,
				Str market_mdl 		as '*Market Model'			=	':Energy Forward - Mkt', 
				Str theo_mdl		as '*Theoretical Model'		=	':Energy Forward - Theo'
				
				//Table stream.Link(id) 							= 	Commodities.Stream [ trds.TRADEID ]
				//Str stream			 							= 	ExtVer ( ExtHor ( Commodities.Stream [ trds.TRADEID ] ) )
				}'''

        objpsr = TendrilObjectParser(functionLibrary=Functions)
        try:
            obj = objpsr.ParseObject(objtest)
        except ParseException as p:
            print(p)
