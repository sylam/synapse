import operator


class Environment(object):
    def __init__(self, logger=None):
        # Reserved words the whole world can see
        self.Rundate = ''
        self.NextRundate = ''
        self.Sequence = None
        self.Rownum = 0
        self.RowObjnum = 0
        self.JustifyLeft = '-'
        self.JustifyRight = ''
        self.DataTypeNumber = 'd'
        self.DataTypeString = 's'
        self.Valid = 0
        self.Warning = 1
        self.Error = 2
        self.Fatal = 3
        self.LoopCounter = 0
        self.NULL = None
        self.LocalParam = {}
        self.VirtualModel = {}
        self.sum = operator.add
        self.product = operator.mul
        self.exp = operator.pow
        self.Eq = operator.eq
        self.Neq = operator.ne
        self.LessThan = operator.lt
        self.GreaterThan = operator.gt
        self.GrThanEqual = operator.ge
        self.LsThanEqual = operator.le

        self.__Finance = {}
        self.__Finance.setdefault('RateIndex', {})

        # for system use
        self.__Logger = logger
        self.__LineNo = 0
        self.__TemplateName = ''
        self.__OutputName = ''

    def QLDayCount(self, daycount):
        return self.__Finance['DayCount'].get(daycount)

    def QLCurrency(self, currency):
        return self.__Finance['Currency'].get(currency)

    def QLCalendar(self, currency):
        return self.__Finance['Calendar'].get(currency)

    def ResetLocals(self):
        # place to reset local variables but keep global state
        self.VirtualModel = {}

    def GetVirtualModel(self, datablock):
        virtualblockname = '%s(%s)' % (','.join(datablock.KeyMap.keys()), ','.join(datablock.DataMap.keys()))
        self.VirtualModel.setdefault(virtualblockname, datablock.copy())
        return self.VirtualModel[virtualblockname]

    def SetLogger(self, logger):
        self.__Logger = logger

    def SetCurrentTemplateName(self, name):
        self.__TemplateName = name

    def SetCurrentOutputName(self, name):
        self.__OutputName = name

    def LogMessage(self, text):
        if self.__Logger:
            self.__Logger('"%s":"%s"' % (self.__OutputName, self.__TemplateName), text)

    def ProcessErrorLevel(self, errorlevel, field, validationName):
        if errorlevel == self.Warning:
            self.LogMessage('Warning - %s has failed validation check [%s]' % (field, validationName))
            return self.Warning
        elif errorlevel == self.Error:
            self.LogMessage('Error - %s has failed validation check [%s]' % (field, validationName))
            return self.Error
        elif errorlevel == self.Fatal:
            self.LogMessage('Fatal - %s has failed validation check [%s]' % (field, validationName))
            return self.Fatal
        else:
            self.LogMessage('Unknown ErrorLevel %s defined for validation check [%s] - Ignoring and continuing' % (
            errorlevel, validationName))
        return self.Valid
