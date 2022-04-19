#usr/bin/env python

#import wxversion
#wxversion.select('2.8-msw-ansi',optionsRequired=True)

import wx
import wx.adv
import wx.html
import wx.stc as stc
import wx.grid as gridlib

import wx.lib.agw.aui as aui

import images

import sys
import time
import copy

import os
import inspect
from synapse import GE_Editor

from optparse import OptionParser
from pyparsing import ParseException

from synapse.GE_Exception import *
from synapse.GE_DBInstance import Context, move, setup
from synapse.GE_Environment import Environment
from synapse.GE_DataFunctions import InstanceFunctionCatalog, ModelFunctionCatalog

__version__='Synapse ver 0.1'

DefaultEditorText = '''
	Hello.
	This is a text editor
	
	Cursor movement:     Arrow keys or mouse left click 
	Beginning of line:   Home
	End of line:         End
	Beginning of buffer: Control-Home
	End of the buffer:   Control-End
	Select text:         Hold down Shift while moving the cursor
	Copy:                Control-Insert, Control-C
	Cut:                 Shift-Delete,   Control-X
	Paste:               Shift-Insert,   Control-V
	Find:                Control-F (buggy)
	Find/Replace:        Control-R (buggy)
	
	The Address pane is where you set things up.
	The Instance pane is where you execute things.
	
	Basically:
				Control-Backspace will try to revert changes in this editor.
				Control-Enter will try to update changes made here.
				
				Right Click to pop up a context menu on either Address or
				Instance pane.
	Good Luck.			
	'''

TreeColours = {
	'Rule':			wx.Colour(250, 50, 10),
	'SoftRule':		wx.Colour(50, 150, 10),
	'Template':		wx.Colour(100, 100, 245),
	'SchemaField':	wx.Colour(245, 100, 100),
	'SchemaDup':	wx.Colour(100, 100, 245),
	'SchemaTrans':	wx.Colour(0,0,0)
	}

#Useful functions for setting file related stuff
def FileDialog(parent, log, workingdir, dest, description, wildcard, dialogStyle = wx.FD_OPEN | wx.FD_CHANGE_DIR):
	dlg = wx.FileDialog(
		parent, message="Choose a(n) %s" % description,
		defaultDir=workingdir,
		defaultFile="",
		wildcard=wildcard,
		style=dialogStyle
		)
	dest = None
	result = dlg.ShowModal()
	if result == wx.ID_OK: dest = dlg.GetPath()
	dlg.Destroy()
	return dest

class TextDialog(wx.Dialog):
	def __init__(self, parent, ID, title, schema={}, size=wx.DefaultSize, pos=wx.DefaultPosition,
					style=wx.DEFAULT_DIALOG_STYLE, verticalChoices=False ):

		wx.Dialog.__init__(self)
		self.SetExtraStyle(wx.DIALOG_EX_CONTEXTHELP)
		self.Create(parent, ID, title, pos, size, style)

		sizer = wx.BoxSizer(wx.VERTICAL)
		self.fields 				= {}
		self.choicelist_population 	= {}

		label = wx.StaticText(self, -1, schema['title'])
		sizer.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)

		if 'choicelist' in schema:
			if not verticalChoices: box = wx.BoxSizer(wx.HORIZONTAL)
			for key,item in sorted(schema['choicelist'].items()):
				if verticalChoices: box = wx.BoxSizer(wx.HORIZONTAL)
				label = wx.StaticText(self, -1, key)
				box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
				choice = wx.Choice(self, -1, size=item['size'], choices = item['choices'])
				box.Add(choice, 1, wx.ALIGN_CENTRE|wx.ALL, 5)
				self.fields.setdefault('choicelist',{}).setdefault(key,choice)
				if 'populates' in item:
					self.choicelist_population[ item['populates'] ] = item['rule']
					self.Bind(wx.EVT_CHOICE, self.EvtChoice, self.fields['choicelist'][key])
				if verticalChoices: sizer.Add(box, 0, wx.GROW|wx.ALL, 5)
			if not verticalChoices: sizer.Add(box, 0, wx.GROW|wx.ALL, 5)

		for key, item in sorted(schema.get('textfields',{}).items()):
			box = wx.BoxSizer(wx.HORIZONTAL)

			label = wx.StaticText(self, -1, key)
			box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)

			text = wx.TextCtrl(self, -1, "", size=item['size'])
			text.SetHelpText(item['field'])
			text.SetValue(item['default'])
			box.Add(text, 1, wx.ALIGN_CENTRE|wx.ALL, 5)
			self.fields.setdefault('textfields',{}).setdefault(key,text)

			sizer.Add(box, 0, wx.GROW|wx.ALL, 5)

		for key, item in sorted(schema.get('stc',{}).items()):
			box = wx.BoxSizer(wx.VERTICAL)
			label = wx.StaticText(self, -1, key)
			box.Add(label, 0, wx.ALIGN_CENTRE|wx.ALL, 5)
			stc_ctrl = MyEditor( self, -1, pos=wx.DefaultPosition, size=item['size'], style=0, statusBar=None, myView=parent.view)

			stc_ctrl.SetText(item['default'])
			parent.view.SetUpEditor(item['setup'], editor=stc_ctrl )
			box.Add(stc_ctrl, 1, wx.GROW|wx.ALL, 5)
			self.fields.setdefault('stc', {}).setdefault(key, stc_ctrl)

			sizer.Add(box, 0, wx.GROW|wx.ALL, 5)

		line = wx.StaticLine(self, -1, size=(20,-1), style=wx.LI_HORIZONTAL)
		sizer.Add(line, 0, wx.GROW|wx.RIGHT|wx.TOP, 5)

		btnsizer = wx.StdDialogButtonSizer()

		if wx.Platform != "__WXMSW__":
			btn = wx.ContextHelpButton(self)
			btnsizer.AddButton(btn)

		btn = wx.Button(self, wx.ID_OK)
		btn.SetHelpText("The OK button tries to complete the action")
		btn.SetDefault()
		btnsizer.AddButton(btn)

		btn = wx.Button(self, wx.ID_CANCEL)
		btn.SetHelpText("Cancels this action")
		btnsizer.AddButton(btn)
		btnsizer.Realize()

		sizer.Add(btnsizer, 0, wx.ALL, 5)

		self.SetSizer(sizer)
		sizer.Fit(self)
		#populate defaults
		if 'choicelist' in schema: self.initChoiceDefaults(schema)

	def initChoiceDefaults(self, schema):
		for key, item in [(k,i) for k,i in self.fields['choicelist'].items() if schema['choicelist'][k]['default'] and k not in self.choicelist_population]:
			item.SetStringSelection( schema['choicelist'][key]['default'] )
			if 'populates' in schema['choicelist'][key]:
				for dependant in schema['choicelist'][key]['rule'](schema['choicelist'][key]['default']):
					self.fields['choicelist'][ schema['choicelist'][key]['populates'] ].Append(dependant)
		for key, item in [(k,i) for k,i in self.fields['choicelist'].items() if schema['choicelist'][k]['default'] and k in self.choicelist_population]:
			item.SetStringSelection( schema['choicelist'][key]['default'] )

	def EvtChoice(self, event):
		for key, value in self.choicelist_population.items():
			self.fields['choicelist'][key].Clear()
			for item in value(event.GetString()):
				self.fields['choicelist'][key].Append(item)

	def GetValues(self, key):
		if key=='textfields':
			return dict([(k,v.GetValue()) for k,v in self.fields.get('textfields',{}).items()])
		elif key=='choicelist':
			return dict([(k,v.GetStringSelection()) for k,v in self.fields.get('choicelist',{}).items()])
		elif key=='stc':
			return dict([(k,v.GetText()) for k,v in self.fields.get('stc',{}).items()])

class OutputTable(gridlib.GridTableBase):
	def __init__(self):
		gridlib.GridTableBase.__init__(self)
		self.odd=gridlib.GridCellAttr()
		self.odd.SetBackgroundColour("light blue")
		self.even=gridlib.GridCellAttr()
		self.even.SetBackgroundColour("white")

		self.colLabels = ['ID', 'Description', 'Hint']

		self.data = [
					[1011, "The Session Window", "Ctrl-Dbl Click a <NULL TEMPLATE> and you load it from disk. Select it by left clicking and then left click it again and you rename it"],
					[1012, "The Session Window", "You can only add new templates as the last item in the tree (leaf nodes)"],
					[1013, "The Schema Window", "You can only delete schema items if no row objects (if any) reference them - you can delete entire sections though"],
					[1014, "The Schema Window", "Elements in red are strictly field references. Black items are transformations. Blue Items are Names that have been duplicated"],
					]

	def ResetView(self, newdata, newcols, parentGrid):
		"""Trim/extend the control's rows and update all values"""
		currentRows, currentColumns = self.GetNumberRows(), self.GetNumberCols()

		#update the stuff
		self.data, self.colLabels = newdata, newcols
		parentGrid.BeginBatch()
		for current, new, delmsg, addmsg in [
				(currentRows, self.GetNumberRows(), gridlib.GRIDTABLE_NOTIFY_ROWS_DELETED, gridlib.GRIDTABLE_NOTIFY_ROWS_APPENDED),
				(currentColumns, self.GetNumberCols(), gridlib.GRIDTABLE_NOTIFY_COLS_DELETED, gridlib.GRIDTABLE_NOTIFY_COLS_APPENDED),
				]:
			if new < current:
				msg = gridlib.GridTableMessage ( self, delmsg, new, current-new )
				parentGrid.ProcessTableMessage(msg)
			elif new > current:
				msg = gridlib.GridTableMessage ( self, addmsg, new-current )
				parentGrid.ProcessTableMessage(msg)
		self.UpdateValues(parentGrid)
		self.AutosizeLabels(parentGrid)
		parentGrid.EndBatch()

		# The scroll bars aren't resized (at least on windows)
		# Jiggling the size of the window rescales the scrollbars
		h,w = parentGrid.GetSize()
		parentGrid.SetSize((h+1, w))
		parentGrid.SetSize((h, w))
		parentGrid.ForceRefresh()

	def UpdateValues( self, parentGrid ):
		"""Update all displayed values"""
		msg = gridlib.GridTableMessage(self, gridlib.GRIDTABLE_REQUEST_VIEW_GET_VALUES)
		parentGrid.ProcessTableMessage(msg)

	def AutosizeLabels(self, parentGrid ):
		# Common setup.

		devContext = wx.ScreenDC()
		devContext.SetFont(parentGrid.GetLabelFont())

		# First do row labels.
		maxWidth = 0
		curRow = parentGrid.GetNumberRows() - 1
		while curRow >= 0:
			curWidth = devContext.GetTextExtent("M%s"%(parentGrid.GetRowLabelValue(curRow)))[0]
			if curWidth > maxWidth:
				maxWidth = curWidth
			curRow = curRow - 1
		parentGrid.SetRowLabelSize(maxWidth)

		# Then column labels.
		maxHeight = 0
		curCol = parentGrid.GetNumberCols() - 1
		while curCol >= 0:
			(w,h,d,l) = devContext.GetFullTextExtent(parentGrid.GetColLabelValue(curCol))
			parentGrid.SetColSize(curCol,w+10)
			curHeight = h + d + l + 4
			if curHeight > maxHeight:
				maxHeight = curHeight
			curCol = curCol - 1
		parentGrid.SetColLabelSize(maxHeight)

	def GetAttr(self, row, col, kind):
		attr = [self.even, self.odd][row % 2]
		attr.IncRef()
		return attr

	# This is all it takes to make a custom data table to plug into a
	# wxGrid.  There are many more methods that can be overridden, but
	# the ones shown below are the required ones.  This table simply
	# provides strings containing the row and column values.

	def GetNumberRows(self):
		return len(self.data) + 1

	def GetNumberCols(self):
		return max( [len(x) for x in self.data] )

	def IsEmptyCell(self, row, col):
		try:
			return not self.data[row][col]
		except IndexError:
			return True

	def GetValue(self, row, col):
		try:
			return self.data[row][col]
		except IndexError:
			return ''

	def SetValue(self, row, col, value):
		#do nothing
		pass

	def GetColLabelValue(self, col):
		return self.colLabels[col]

class CustomStatusBar(wx.StatusBar):
	def __init__(self, parent):
		wx.StatusBar.__init__(self, parent, wx.NewIdRef())
		self.SetFieldsCount(3)
		self.SetStatusWidths([-1,50,50])

	def setText(self, txt):
		self.SetStatusText(txt, 0)

	def setRowCol(self, row, col):
		self.SetStatusText("%04d" % row, 1)
		self.SetStatusText("%04d" % col, 2)

class MyEditor(stc.StyledTextCtrl):
	def __init__(self, parent, id, pos=wx.DefaultPosition, size=wx.DefaultSize, style=0, statusBar=None, myView=None):

		stc.StyledTextCtrl.__init__(self, parent, id)

		self.autocompstops 	= '([{}]),/ \|+-*=\t'
		self.currentline	= None
		self.currentpos		= 0
		self.StatusBar 		= statusBar
		self.parent 		= parent
		self.view   		= myView

		#Events
		self.Bind(wx.EVT_KEY_UP, self.OnKeyUp)
		self.Bind(wx.EVT_KEY_DOWN, self.OnKeyDown)
		self.Bind(wx.EVT_LEFT_UP, self.OnFocus)
		self.Bind(wx.EVT_LEFT_DOWN, self.OnFocus)
		self.Bind(wx.EVT_ACTIVATE, self.OnActivate)
		self.Bind(wx.EVT_CHAR, self.OnChar)
		self.Bind(wx.EVT_KILL_FOCUS, self.OnLoseFocus)
		self.Bind(stc.EVT_STC_DOUBLECLICK, self.OnDblClick)

		#Find and Replace
		self.Bind(wx.EVT_FIND, self.OnFind)
		self.Bind(wx.EVT_FIND_NEXT, self.OnFind)
		self.Bind(wx.EVT_FIND_REPLACE, self.OnFind)
		self.Bind(wx.EVT_FIND_REPLACE_ALL, self.OnFind)
		self.Bind(wx.EVT_FIND_CLOSE, self.OnFindClose)

		self.SetStyle('cpp')

		self.hotkeys 		= []
		self.catalogue 		= None
		self.calltips 		= {}
		self.functions		= []
		self.LastControl 	= None

	def OnShowFind(self):
		data = wx.FindReplaceData()
		dlg = wx.FindReplaceDialog(self, data, "Find")
		dlg.data = data  # save a reference to it...
		dlg.Show(True)

	def OnShowFindReplace(self):
		data = wx.FindReplaceData()
		dlg = wx.FindReplaceDialog(self, data, "Find & Replace", wx.FR_REPLACEDIALOG)
		dlg.data = data  # save a reference to it...
		dlg.Show(True)

	def OnFind(self, evt):

		def ProcessPostion(location, length, replacetext=None):
			if location>0:
				if down:
					self.GotoPos( location + ( replacetext and len(replacetext) or length ) )
				else:
					self.GotoPos(location)
				self.SearchAnchor()
				self.SetSelection( location, location+length )
				if replacetext:
					self.ReplaceSelection(replacetext)
			else:
				self.view.LogMessage('Find/Replace','String not found')

		et 			= evt.GetEventType()
		flags 		= evt.GetFlags()
		down 		= (flags & wx.FR_DOWN)
		findtext	= evt.GetFindString()
		stcflags 	= 0
		stcflags 	|= flags & wx.FR_WHOLEWORD and stc.STC_FIND_WHOLEWORD
		stcflags 	|= flags & wx.FR_MATCHCASE and stc.STC_FIND_MATCHCASE

		if et == wx.wxEVT_COMMAND_FIND:
			range = down and (self.GetCurrentPos(), self.GetTextLength()) or (self.GetTextLength(), self.GetCurrentPos())
			ProcessPostion ( self.FindText ( range[0], range[1], findtext, stcflags ), len(findtext) )
		elif et == wx.wxEVT_COMMAND_FIND_NEXT:
			ProcessPostion ( down and self.SearchNext(stcflags, findtext) or self.SearchPrev(stcflags, findtext), len(findtext) )
		elif et == wx.wxEVT_COMMAND_FIND_REPLACE:
			ProcessPostion ( down and self.SearchNext(stcflags, findtext) or self.SearchPrev(stcflags, findtext), len(findtext), evt.GetReplaceString() )
		elif et == wx.wxEVT_COMMAND_FIND_REPLACE_ALL:
			self.SetText( self.GetText().replace(findtext, evt.GetReplaceString()) )

	def OnFindClose(self, evt):
		evt.GetDialog().Destroy()

	def SetStyle(self, style):

		styles = {'helv': 'Lucida Console', 'mono': 'Courier New', 'lnsize': 6, 'backcol': '#FFFFFF', 'size': 10}

		#also nice
		self.StyleSetSpec( stc.STC_STYLE_BRACELIGHT,"fore:#0000FF,back:#FFFFB9,bold"%styles)
		self.StyleSetSpec( stc.STC_STYLE_BRACEBAD,"fore:#FF0000,back:#FFFFB9,bold"%styles)

		self.StyleSetSpec( stc.STC_STYLE_DEFAULT, 	"face:%(mono)s,size:%(size)d"%styles)
		self.StyleSetSpec( stc.STC_STYLE_LINENUMBER,	"size:%(lnsize)s"%styles)

		if style=='cpp':
			self.StyleSetSpec( stc.STC_C_COMMENT,			"fore:#008040,back:#EAFFEA"%styles)
			self.StyleSetSpec( stc.STC_C_COMMENTLINE,		"fore:#008040,back:#EAFFEA,size:8"%styles)
			self.StyleSetSpec( stc.STC_C_NUMBER,			"fore:#0076AE"%styles)
			self.StyleSetSpec( stc.STC_C_WORD,				"bold,fore:#008040"%styles)
			self.StyleSetSpec( stc.STC_C_WORD2, 			"fore:#F000F0,bold" % styles )
			self.StyleSetSpec( stc.STC_C_STRING,			"fore:#800080"%styles)
			self.StyleSetSpec( stc.STC_C_CHARACTER,			"fore:#800040"%styles)
			self.StyleSetSpec( stc.STC_C_PREPROCESSOR,		"fore:#808000"%styles)
			self.StyleSetSpec( stc.STC_C_OPERATOR,			"bold"%styles)
			self.StyleSetSpec( stc.STC_C_STRINGEOL,			"back:#FFD5FF"%styles)
			self.StyleSetSpec( stc.STC_C_VERBATIM,			"fore:#8000FF"%styles)
			self.StyleSetSpec( stc.STC_C_IDENTIFIER, 		"fore:#2000F0"%styles )

		elif style=='lisp':
			self.StyleSetSpec( stc.STC_LISP_COMMENT, 	"fore:#008040,back:#EAFFEA"%styles )
			self.StyleSetSpec( stc.STC_LISP_IDENTIFIER,	"fore:#2000F0"%styles )
			self.StyleSetSpec( stc.STC_LISP_KEYWORD,	"fore:#F000F0,bold" % styles )
			self.StyleSetSpec( stc.STC_LISP_NUMBER,		"fore:#0076AE"%styles )
			self.StyleSetSpec( stc.STC_LISP_OPERATOR,	"bold"%styles)
			self.StyleSetSpec( stc.STC_LISP_STRING,		"fore:#800080"%styles)
			self.StyleSetSpec( stc.STC_LISP_STRINGEOL,	"back:#FFD5FF"%styles)
			self.StyleSetSpec( stc.STC_LISP_DEFAULT, 	"face:%(mono)s,size:%(size)d"%styles)

		self.SetLexerLanguage(style)
		self.AutoCompStops(self.autocompstops)
		self.SetEOLMode(stc.STC_EOL_LF)
		self.SetTabWidth(4)

	def Request(self, control, text):
		self.LastControl = control
		self.LastControl.Enable(False)
		self.SetReadOnly(False)
		self.SetText(text)

	def UpdateStatusBar(self):
		self.currentline, self.currentpos = self.GetCurLine()
		if self.StatusBar:
			self.StatusBar.setRowCol(self.GetCurrentLine()+1, self.currentpos)
			self.SetFocus()
			self.Refresh()

	def UpdateCursor(self):
		self.CheckBrace()
		self.UpdateStatusBar()

	def OnActivate(self, evt):
		self.UpdateCursor()
		evt.Skip()

	def OnKeyDown(self, evt):
		self.UpdateCursor()
		evt.Skip()

	def OnKeyUp(self, evt):
		self.UpdateCursor()
		if self.LastControl and evt.m_controlDown==True and self.GetReadOnly()==False:
			if evt.GetKeyCode()==wx.WXK_RETURN:
				if self.LastControl.Check(self.GetText()):
					self.LastControl.Enable(True)
					self.LastControl = None
					self.SetReadOnly(True)
			elif evt.GetKeyCode()==wx.WXK_BACK:
				self.LastControl.Enable(True)
				self.LastControl.Revert()
			self.Refresh()
		evt.Skip()

	def OnFocus(self, evt):
		self.UpdateCursor()
		self.calltips, self.functions = self.UpdateCallTips()
		evt.Skip()

	def OnLoseFocus(self, event):
		self.BraceHighlight ( stc.STC_INVALID_POSITION, stc.STC_INVALID_POSITION )
		event.Skip()

	def UpdateCallTips(self):
		#handle updating the calltips dynamically
		if self.catalogue:
			datafns = [x for x in dir(self.catalogue) if inspect.ismethod(getattr(self.catalogue, x)) and not x.startswith('__') ]
			calltips = dict ( [ ( name, inspect.getdoc(getattr(self.catalogue, name)) ) for name in datafns ] )
		else:
			calltips = {}
			datafns = []
		return calltips, datafns

	def CheckBrace(self):
		curpos = self.GetCurrentPos()
		block = [i for i in range(0, -2, -1) if chr(self.GetCharAt(curpos+i)) in '()[]{}']
		if block:
			p2 = self.BraceMatch(curpos+block[0])
			if p2 == stc.STC_INVALID_POSITION:
				self.BraceBadLight(curpos+block[0])
			else:
				self.BraceHighlight(curpos+block[0], p2)
		else:
			self.BraceHighlight(stc.STC_INVALID_POSITION, stc.STC_INVALID_POSITION)

	def OnDblClick(self, evt):
		text = self.GetSelectedText()
		if text in self.calltips:
			self.CallTipShow ( self.GetCurrentPos(), self.calltips[text] )

	def OnChar(self, evt):
		def GetLastBit(line):
			for i in range(len(line))[::-1]:
				if line[i] in self.autocompstops:
					return line[i+1:]
			return line

		keycode = evt.GetKeyCode ()

		if keycode==6:
			self.OnShowFind()
		elif keycode==18:
			self.OnShowFindReplace()
		else:
			if self.GetReadOnly():
				self.view.LogMessage('Synapse', 'Control is read-only - Select an editable item', statusbar='Editor is ReadOnly')

			if keycode>=0 and keycode<=255:
				c = chr ( keycode )
				if c in ['(', '['] and self.calltips:
					buf=GetLastBit(self.currentline[:self.currentpos])
					if buf in self.calltips:
						self.CallTipShow ( self.GetCurrentPos(), self.calltips[buf] )
				elif c not in self.autocompstops:
					buf=GetLastBit(self.currentline[:self.currentpos]+c)
					w=[x for x in self.hotkeys if x.startswith(buf)]
					if w and not self.AutoCompActive():
						self.AutoCompShow(len(buf)-1,' '.join(sorted(w)))
			evt.Skip()

class MyModelEditor(MyEditor):
	def __init__(self, parent, id, pos=wx.DefaultPosition, size=wx.DefaultSize, style=0, statusBar=None, myView=None):
		MyEditor.__init__(self, parent, id, pos, size, style, statusBar, myView)
		self.catalogue  = self.view.ModelFns
		self.calltips, self.functions = self.UpdateCallTips()
		self.SetKeyWords (0, 'Str Int Real Table RefTo RefBy Link as primary sequence select' )
		self.SetKeyWords (1, ' '.join(self.functions) )
		self.SetKeyWords (3, ' '.join(self.view.envKeyWords) )
		self.SetReadOnly(False)
		# Don't view white space
		self.SetViewWhiteSpace(False)

	def OnFocus(self, evt):
		MyEditor.OnFocus(self, evt)
		self.SetKeyWords (1, ' '.join(self.functions) )

#need to make this a dnd source
class MySQLEditor(wx.Panel):
	def __init__(self, parent, ID, view, pos=wx.DefaultPosition, size=wx.DefaultSize, style=wx.BORDER_SUNKEN ):
		wx.Panel.__init__(self, parent, ID, pos, size, style)

		self.view 		= view
		self.notes   	= MyEditor( self, wx.NewIdRef(), style=wx.SUNKEN_BORDER, statusBar=view.sb, myView=view )
		self.tree 		= wx.TreeCtrl( self, wx.NewIdRef(), wx.DefaultPosition, wx.DefaultSize, wx.TR_HAS_BUTTONS )
		self.root 		= self.tree.AddRoot('Databases')
		self.currentdb  = None

		#set the event overrides
		self.notes.Bind(wx.EVT_KEY_UP, self.OnKeyUp)
		self.tree.Bind (wx.EVT_TREE_ITEM_ACTIVATED , self.EvtChoice)
		self.tree.Bind (wx.EVT_LEFT_DOWN, self.OnLeftDown)

		#setup the tree
		self.tree.SetItemData(self.root, None)
		box 	= wx.BoxSizer(wx.HORIZONTAL)

		box.Add(self.tree, 3 , wx.ALL|wx.GROW, 1)
		box.Add(self.notes, 7, wx.ALL|wx.GROW, 1)

		self.SetSizer(box)
		self.SetAutoLayout(1)

		self.Refresh()

	def Refresh(self):
		allfields 		= set()
		tables			= {}

		self.tree.DeleteChildren(self.root)
		for dbname, value in self.view.controller.Addresses.items():
			dbnode = self.tree.AppendItem(self.root, dbname)
			if value.address_type!='Directory':
				for name, table in sorted( value.schema.meta.tables.items() ):
					node = self.tree.AppendItem(dbnode, table.name)
					self.tree.SetItemData(node, ('table',table))
					for col in table.columns:
						child = self.tree.AppendItem(node, '%s [%s]' % (col.name,col.type) )
						self.tree.SetItemData(child, ('field',col))
						allfields.add(col.name)
		allfields.update(tables.keys())
		self.notes.hotkeys = list(allfields)
		self.notes.SetKeyWords (0, ' '.join(list(allfields)) )
		self.notes.SetKeyWords (3, ' '.join(tables.keys()) )
		self.tree.Expand(self.root)
		wx.Panel.Refresh(self)

	def OnLeftDown(self, event):
		pt = event.GetPosition()
		item, flags = self.tree.HitTest(pt)
		if item:
			self.tree.SelectItem(item)
			data = wx.TextDataObject( self.tree.GetItemText(item) )
			dropSource = wx.DropSource(self.tree)
			dropSource.SetData( data )
			#don't really care about the result
			result = dropSource.DoDragDrop(wx.Drag_AllowMove)

		event.Skip()

	def EvtChoice(self, event):
		item = event.GetItem()
		parent = self.tree.GetItemParent(item)
		if parent==self.root:
			if self.currentdb:
				self.tree.SetItemBold(self.currentdb, False)
			self.currentdb = item
			self.view.LogMessage('SQL Editor', 'Active db set to [%s]' % self.tree.GetItemText(self.currentdb))
			self.tree.SetItemBold(self.currentdb, True)

	def OnKeyUp(self, event):
		self.notes.UpdateCursor()
		if event.GetKeyCode()==wx.WXK_RETURN and event.controlDown==True:
			text = self.notes.GetSelectedText()
			if self.currentdb!=None:
				if text:
					try:
						rows = self.view.controller.Addresses[self.tree.GetItemText(self.currentdb)].schema.engine.execute(text)
					except SQLErrorException as e:
						wx.Bell()
						self.view.LogMessage('SQL Editor', str(e), statusbar='SQL Error')
					except:
						wx.Bell()
						self.view.LogMessage('SQL Editor', 'UnHandled Exception',statusbar='UnHandled SQL Exception')
						self.view.LogTraceBack(GE_Editor.FunctionError(sys.exc_info()))
					else:
						if rows:
							self.view.GetOutputWindow().ResetView( rows.fetchall(), rows.keys, self.view.GetGridWindow())
						else:
							self.view.LogMessage('SQL Editor', 'No Rows returned from query')
			else:
				self.view.LogMessage('SQL Editor', 'Please dbl click on a database to make it active')
		event.Skip()

class MyTextDropTarget(wx.TextDropTarget):
	def __init__(self, window, log):
		wx.TextDropTarget.__init__(self)
		self.log = log
		self.window = window
		self.data = wx.TextDataObject()
		self.SetDataObject(self.data)
		self.PreviousItem=None

	def OnDragOver(self, x, y, d):
		item, flags = self.window.HitTest((x,y))
		if item and self.window.dndTarget and item!=self.PreviousItem and item!=self.window.GetRootItem() and self.window.GetItemText(item)!='Synapse':
			if self.PreviousItem and self.window.IsExpanded(self.PreviousItem):
				self.window.Collapse(self.PreviousItem)
			if not self.window.IsExpanded(item):
				self.window.Expand(item)
			self.PreviousItem=item
		return wx.DragCopy

	def OnData(self, x, y, d):
		if self.GetData() and self.window.dndTarget:
			item = self.window.CreateNode(self.PreviousItem, self.window.GetItemText(self.PreviousItem)+'Test', self.data.GetText())
			self.window.HotItem = item
			if self.window.Check('elementdata', self.data.GetText()):
				self.log.LogMessage( 'Schema(Dnd)',"New Variable [%s] added" % ( self.window.GetItemText(self.PreviousItem)+'Test' ) )
			else:
				self.log.LogMessage( 'Schema(Dnd)',"New Variable [%s] Not added" % ( self.window.GetItemText(self.PreviousItem)+'Test' ) )
				self.window.Delete( item )
			self.window.Refresh()

class AddressTreeControl(wx.TreeCtrl):
	def __init__(self, parent, ID, name, view, style=wx.TR_HAS_BUTTONS):
		wx.TreeCtrl.__init__(self, parent, ID, wx.Point(0, 0), wx.Size(160, 250), style | wx.TR_DEFAULT_STYLE | wx.NO_BORDER)

		imglist = wx.ImageList(16, 16, True, 9)
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_HELP_SETTINGS, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_HELP_BOOK, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_HELP_PAGE, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_LIST_VIEW, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_REPORT_VIEW, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_GO_FORWARD, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_FOLDER, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_EXECUTABLE_FILE, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_FOLDER_OPEN, wx.ART_OTHER, wx.Size(16, 16)))
		self.AssignImageList(imglist)

		self.root 		= self.AddRoot(name, 0)
		self.view 		= view
		self.HotItem 	= None
		#wx window events
		self.Bind (wx.EVT_CONTEXT_MENU, self.OnContextMenu)
		self.Bind (wx.EVT_LEFT_DOWN, self.OnLeftDown)
		self.Bind (wx.EVT_RIGHT_DOWN, self.OnRightDown)
		self.Bind (wx.EVT_TREE_ITEM_ACTIVATED , self.EvtChoice)

		self.alias_schema = {
							'title':'Modify Alias',
							'textfields':{
									'name':{'default':'name of alias', 'size':(200,-1), 'field':'text only - no special characters - case sensitive'},
									'resource':{'default':'name of resource (either database table or file)', 'size':(200,-1), 'field':'case sensitive'}
									}
							}
		self.link_schema = 	{
							'title':'Modify Associations',
							'textfields':{
									'field1':{'default':'name of field on alias1', 'size':(200,-1), 'field':'case sensitive'},
									'field2':{'default':'name of field on alias2', 'size':(200,-1), 'field':'case sensitive'},
									'hint'  :{'default':'1', 'size':(200,-1), 'field':'integer'}
									},
							'choicelist':{
								'alias1': {'default':None, 'size':(100,-1), 'choices':[]},
								'alias2': {'default':None, 'size':(100,-1), 'choices':[]},
								'jointype': {'default':'Inner', 'size':(100,-1), 'choices':['Inner','Left Outer','Right Outer']}
								}
							}
		self.cond_schema = 	{
							'title':'Modify Conditions',
							'textfields':{
										'name':{'default':'name of Condition', 'size':(500,-1), 'field':'text only - no special characters - case sensitive'},
										},
							'choicelist':{
										'type': {'default':None, 'size':(500,-1), 'choices':['Filter','Instruction']},
										},
							'stc':		{
										'expression':{'default':'Expression for condition', 'size':(500,500), 'field':'text only - no special characters - case sensitive', 'setup':None}
										}
							}
		self.modelgroup_schema = {
							'title':'Modify ModelGroup',
							'textfields':{
									'name':{'default':'name of modelgroup', 'size':(200,-1), 'field':'text only - no special characters - case sensitive'},
									'description':{'default':'optional description', 'size':(200,-1), 'field':'optional - but recommended'}
									}
							}
		#binds
		self.popupID1 = wx.NewIdRef()
		self.popupID2 = wx.NewIdRef()
		self.popupID3 = wx.NewIdRef()
		self.popupID4 = wx.NewIdRef()

		self.popupID5 = wx.NewIdRef()
		self.popupID6 = wx.NewIdRef()
		self.popupID7 = wx.NewIdRef()
		self.popupID8 = wx.NewIdRef()
		self.popupID9 = wx.NewIdRef()

		self.Bind(wx.EVT_MENU, self.OnPopupAdd, 		id=self.popupID1)
		self.Bind(wx.EVT_MENU, self.OnPopupRemove, 		id=self.popupID2)
		self.Bind(wx.EVT_MENU, self.OnPopupModify, 		id=self.popupID3)
		self.Bind(wx.EVT_MENU, self.OnPopupAddModel,	id=self.popupID4)

		self.Bind(wx.EVT_MENU, self.OnPopupAddSQLiteDataSource,		id=self.popupID5)
		self.Bind(wx.EVT_MENU, self.OnPopupAddOracleDataSource,		id=self.popupID6)
		self.Bind(wx.EVT_MENU, self.OnPopupAddSQLServerDataSource,	id=self.popupID7)
		self.Bind(wx.EVT_MENU, self.OnPopupAddSybaseDataSource,		id=self.popupID9)
		self.Bind(wx.EVT_MENU, self.OnPopupAddDirectory,			id=self.popupID8)

	def _completeAddress(self, node, addressname):
		def _completeModelGroups(parent_node, group):
			for model_name, model_data in sorted(group.models.items()):
				model_node = self.AppendItem ( parent_node, model_name, 7 )
				self.SetItemData ( model_node, ('Model', group, model_data.source ) )
			for child_group, child_data in sorted(group.children.items()):
				child_node = self.AppendItem ( parent_node, child_group, 6 )
				self.SetItemData ( child_node, ('ModelGroup', child_data, group) )
				_completeModelGroups(child_node, child_data )

		addresses = self.GetItemData(self.root)
		for nodedata,image_num in [('Aliases',2),('Associations',3),('Conditions',4),('ModelGroupRoot',8)]:
			new_node = self.AppendItem(node, nodedata, image_num)
			self.SetItemData ( new_node, (nodedata, addressname ) )
			if nodedata=='Aliases':
				for alias, alias_data in sorted(addresses[addressname].aliases.items()):
					alias_node = self.AppendItem ( new_node, alias, 5 )
					self.SetItemData ( alias_node, ('alias', alias_data.resource_name ) )
			elif nodedata=='Associations':
				for (a1,f1,a2,f2), link_data in sorted(addresses[addressname].links.items()):
					link_node = self.AppendItem ( new_node, '%s.%s = %s.%s%s' % ( a1,f1,a2,f2,' (+)' if link_data.jointype==1 else (' (-)' if link_data.jointype==2 else '') ) , 5 )
					self.SetItemData ( link_node, ( 'association', a1, f1, a2, f2 ) )
			elif nodedata=='Conditions':
				for cond, cond_data in sorted(addresses[addressname].conditions.items()):
					cond_node = self.AppendItem ( new_node, cond, 5 )
					self.SetItemData ( cond_node, ('condition', cond_data.type, cond_data.value ) )
			elif nodedata=='ModelGroupRoot':
				group = addresses[addressname].modelGroupRoot
				self.SetItemData ( new_node, ('ModelGroupRoot', group, None ) )
				_completeModelGroups(new_node, group)

	def SetConfigTree(self, addresses):
		self.DeleteChildren(self.root)
		self.SetItemData(self.root, addresses)
		for key in sorted(addresses.keys()):
			node = self.AppendItem(self.root, key, 1)
			self.SetItemData(node, ('address', addresses[key] ) )
			self._completeAddress(node, key)

	def OnRightDown(self, event):
		pt = event.GetPosition();
		item, flags = self.HitTest(pt)
		if item:
			self.SelectItem(item)
		event.Skip()

	def OnLeftDown(self, event): #Dnd code here
		pt = event.GetPosition()
		item, flags = self.HitTest(pt)
		if item:
			data = wx.TextDataObject( self.GetItemText(item) )
			dropSource = wx.DropSource(self)
			dropSource.SetData( data )
			self.dndTarget = False
			#don't really care about the result
			result = dropSource.DoDragDrop(wx.Drag_AllowMove)
			self.dndTarget = True
			self.SelectItem(item)

		event.Skip()

	def EvtChoice(self, event):
		item = event.GetItem()
		data = self.GetItemData(item)
		if data[0]=='Model':
			if self.HotItem: self.SetItemBold(self.HotItem, False)
			self.HotItem = item
			self.SetItemBold(self.HotItem, True)
			self.view.Request(self, data[2],readonly=True)
		event.Skip()

	def Check(self, text):
		addresses 	= self.GetItemData(self.root)
		data 		= self.GetItemData(self.HotItem)
		name 		= self.GetItemText(self.HotItem)

		try:
			group = data[1] if data[0] == 'Model' else (data[2].children[name] if data[2] else data[1])
			existing_m = group.models.keys()
			modelname = self.view.controller.AddModel(text, group)
		except ParseException as e:
			wx.Bell()
			self.view.LogMessage('Model Editor', 'Parsing Error - [%s]' % str(e), statusbar='Parsing Error')
			return False
		except ModelNameNotFoundException as e:
			wx.Bell()
			self.view.LogMessage('Model Editor', 'Model name [%s] Could not be found - Check the Model Group Hierarchy' % str(e), statusbar='Model name Undefined')
			return False
		except FunctionNotFoundException as e:
			wx.Bell()
			self.view.LogMessage('Model Editor', 'Function name [%s] Could not be found - Check the Function Hierarchy' % str(e), statusbar='Function name Undefined')
			return False
		except:
			wx.Bell()
			self.view.LogMessage('Model Editor', 'UnHandled Exception', statusbar='UnHandled Exception')
			self.view.LogTraceBack(GE_Editor.FunctionError(sys.exc_info()))
			return False
		else:
			self.view.LogMessage('Model Editor', 'Updated [%s]' % self.GetItemText(self.HotItem), statusbar='Item Updated')
			model_node = self.AppendItem(self.HotItem if data[0] in ('ModelGroup', 'ModelGroupRoot') else self.GetItemParent(self.HotItem),  modelname, 7) if (modelname not in existing_m) else self.HotItem
			self.SetItemData(model_node, ('Model', group, text))
			self.SetItemBold(self.HotItem, False)
			self.HotItem = None
			return True

	def Revert(self):
		if self.HotItem:
			self.view.LogMessage('Model Editor', 'Reverting [%s]' % self.GetItemText(self.HotItem), statusbar='Item Reverted')
			self.SetItemBold(self.HotItem, False)
			self.HotItem=None

	def OnContextMenu(self, event):
		# make a menu
		self.SetFocus()
		item = self.GetSelection()
		if item:
			self.Expand(item)
			data = self.GetItemData(item)
			name = self.GetItemText(item)
			menu = wx.Menu()

			if item==self.root:
				menu.Append(self.popupID5, "Add SQLite DataSource")
				menu.Append(self.popupID6, "Add Oracle DataSource")
				menu.Append(self.popupID7, "Add SQL Server DataSource")
				menu.Append(self.popupID9, "Add Sybase DataSource")
				menu.Append(self.popupID8, "Add File Directory DataSource")
			elif data and data[0] in ('Aliases','Associations', 'Conditions'):
				menu.Append(self.popupID1, "Add Item")
			else:
				if data[0] in ('ModelGroup','ModelGroupRoot'):
					menu.Append(self.popupID4, "Add Model")
					menu.Append(self.popupID1, "Add SubGroup")
				if data[0] not in ('address', 'association', 'ModelGroupRoot'):
					menu.Append(self.popupID3, "Modify Item")
				if data[0] != 'ModelGroupRoot':
					menu.Append(self.popupID2, "Remove Item")

			self.ContextPacket = (item, data, name)
			self.Unselect()
			self.PopupMenu(menu)
			menu.Destroy()
			self.SelectItem(item)

	def LoadDataSource(self, config, dbtype):

		def TextDialog(description, heading):
			value = None
			dlg = wx.TextEntryDialog( self, description, heading )
			if dlg.ShowModal() == wx.ID_OK:
				value = dlg.GetValue()
			dlg.Destroy()
			return value

		dataOK=True
		for key, value in config.items():
			if value[1]=='File':
				description, wildcard = value[2:]
				value[0] = FileDialog(self, self.view.LogMessage, os.getcwd(), None, description, wildcard)
			elif value[1]=='Dir':
				dlg = wx.DirDialog(self, "Choose a directory:", style=wx.DD_DEFAULT_STYLE)
				if dlg.ShowModal() == wx.ID_OK: value[0] = dlg.GetPath()
				dlg.Destroy()
			elif value[1]=='Text':
				value[0] = TextDialog( value[2], 'Synapse' )
			if value[0]==None:
				self.view.LogMessage('Synapse','New DataSource Cancelled by User')
				dataOK=False
				break

		if dataOK:
			addresses = self.GetItemData(self.root)
			item, data, name = self.ContextPacket
			if config['DBName'][0] not in addresses:
				self.view.controller.AddAddress ( config['DBName'][0], dbtype, config['Source'][0] if dbtype!= 'SQLite' else 'sqlite:///'+config['Source'][0][2:], config.get('Schema',[None])[0] )
				dbnode = self.AppendItem(self.root, config['DBName'][0], 1)
				self.SetItemData ( dbnode, ('address', config['DBName'][0] ) )
				self._completeAddress ( dbnode, config['DBName'][0] )
				self.view.UpdateDatabases()
			else:
				errdlg = wx.MessageDialog( self, 'Address names must be unique', 'Address name already exists', wx.OK | wx.ICON_ERROR )
				errdlg.ShowModal()
				errdlg.Destroy()

	def OnPopupAddSQLiteDataSource(self, event):
		config = {'Source':[None, 'File', 'Existing sqlite datasource', "Database files (*.db)|*.db|All files (*.*)|*.*"],
				  'DBName':[None, 'Text', 'Datasource Name']}

		self.LoadDataSource(config, 'SQLite')

	def OnPopupAddSQLServerDataSource(self, event):
		config = {'Source':[None, 'Text', 'Existing SQLServer datasource'],
				  'DBName':[None, 'Text', 'Datasource Name']}

		self.LoadDataSource(config, 'SQLServer')

	def OnPopupAddOracleDataSource(self, event):
		config = {'Source':[None, 'Text', 'Existing Oracle datasource'],
				  'Schema':[None, 'Text', 'Schema Name (Press enter for default)'],
				  'DBName':[None, 'Text', 'Datasource Name']}

		self.LoadDataSource(config, 'Oracle')

	def OnPopupAddSybaseDataSource(self, event):
		config = {'Source':[None, 'Text', 'Existing Sybase ODBC datasource'],
				  'DBName':[None, 'Text', 'Datasource Name']}

		self.LoadDataSource(config, 'Sybase')

	def OnPopupAddDirectory(self, event):
		config = {'Source':[None, 'Dir', 'Existing File Directory'],
				  'DBName':[None, 'Text', 'Datasource Name']}

		self.LoadDataSource(config, 'Directory')

	def	OnPopupModify(self, event):
		item, data, name = self.ContextPacket
		parent = self.GetItemParent(item)
		if data[0]=='alias':
			modalias = copy.deepcopy(self.alias_schema)
			modalias['textfields']['resource']['default'] = data[1]
			del modalias['textfields']['name']
			self.DialogAlais(self.GetItemData(parent)[1], item, 'Modify Alias', modalias, key=name)
		elif data[0]=='condition':
			modcond = copy.deepcopy(self.cond_schema)
			modcond['choicelist']['type']['default'] = self.cond_schema['choicelist']['type']['choices'][ data[1] ]
			modcond['stc']['expression']['default'] = data[2]
			modcond['stc']['expression']['setup'] = self.view.controller.Addresses[self.GetItemData(parent)[1]]
			del modcond['textfields']
			self.DialogCondition(self.GetItemData(parent)[1], item, "Modify Condition", modcond, key=name)
		elif data[0]=='ModelGroup':
			modgroup = copy.deepcopy(self.modelgroup_schema)
			modgroup['textfields']['description']['default'] = data[1].description
			del modgroup['textfields']['name']
			self.DialogModelGroup( data[2], item, "Modify Condition", modgroup, key=name )
		elif data[0]=='Model':
			if self.HotItem: self.SetItemBold(self.HotItem, False)
			addresses 			= self.GetItemData(self.root)
			self.HotItem 		= item
			self.SetItemBold(item, True)

			self.view.SetUpEditor ( data[1].address )
			self.view.SetCodePage ( 0 )
			self.view.Request ( self, data[2] )

		self.Expand(item)

	def DialogAlais(self, address, item, title, schema, key=None):
		dlg = TextDialog(self, -1, title, schema, size=(400, 300), style=wx.DEFAULT_DIALOG_STYLE)
		dlg.CenterOnScreen()
		if dlg.ShowModal()==wx.ID_OK:
			values = dlg.GetValues('textfields')
			self.view.controller.AddAlias(address, values['name'] if not key else key, values['resource'])
			node = item if key else self.AppendItem ( item,  values['name'], 5 )
			self.SetItemData(node, ('alias', values['resource'] ) )
		dlg.Destroy()

	def DialogAssociation(self, address, item, aliases, title, schema):
		dlg = TextDialog(self, -1, title, schema, size=(400, 300), style=wx.DEFAULT_DIALOG_STYLE)
		dlg.CenterOnScreen()
		if dlg.ShowModal()==wx.ID_OK:
			textvalues = dlg.GetValues('textfields')
			choicevalues = dlg.GetValues('choicelist')
			self.view.controller.AddAssociation( address, choicevalues['alias1'], choicevalues['alias2'], textvalues['field1'], textvalues['field2'], int(textvalues['hint']), schema['choicelist']['jointype']['choices'].index( choicevalues['jointype'] ) )
			node = self.AppendItem ( item,  '%s.%s = %s.%s' % ( choicevalues['alias1'], textvalues['field1'], choicevalues['alias2'], textvalues['field2'] ), 5 )
			self.SetItemData(node, ('association', choicevalues['alias1'], textvalues['field1'], choicevalues['alias2'], textvalues['field2'] ) )
		dlg.Destroy()

	def DialogCondition(self, address, item, title, schema, key=None):
		dlg = TextDialog(self, -1, title, schema, size=(600, 400), style=wx.DEFAULT_DIALOG_STYLE)
		dlg.CenterOnScreen()
		code = dlg.ShowModal()
		if code==wx.ID_OK:
			textvalues = dlg.GetValues('textfields')
			stcvalues = dlg.GetValues('stc')
			choicevalues = dlg.GetValues('choicelist')
			try:
				self.view.controller.AddCondition ( address, textvalues['name'] if not key else key, schema['choicelist']['type']['choices'].index(choicevalues['type']), stcvalues['expression'] )
			except ParseException as p:
				dlg.Destroy()
				errdlg = wx.MessageDialog( self, str(p), 'Parsing Error', wx.OK | wx.ICON_ERROR )
				errdlg.ShowModal()
				errdlg.Destroy()
			else:
				node = item if key else self.AppendItem ( item, textvalues['name'], 5 )
				self.SetItemData ( node, ('condition', schema['choicelist']['type']['choices'].index(choicevalues['type']), stcvalues['expression'] ) )
				dlg.Destroy()
				okdlg = wx.MessageDialog(self, 'Successfully parsed and recorded - well done!', 'Success', wx.OK | wx.ICON_INFORMATION)
				okdlg.ShowModal()
				okdlg.Destroy()
		else:
			dlg.Destroy()

	def DialogModelGroup(self, parent, item, title, schema, key=None):
		dlg = TextDialog(self, -1, title, schema, size=(400, 300), style=wx.DEFAULT_DIALOG_STYLE)
		dlg.CenterOnScreen()
		if dlg.ShowModal()==wx.ID_OK:
			values = dlg.GetValues('textfields')
			mod_group = self.view.controller.AddModelGroup(parent, values['name'] if not key else key, values['description'])
			node = item if key else self.AppendItem ( item,  values['name'], 6 )
			self.SetItemData(node, ('ModelGroup', mod_group, parent ) )
		dlg.Destroy()

	def OnPopupAddModel(self, event):
		item, data, name 	= self.ContextPacket

		if self.HotItem: self.SetItemBold(self.HotItem, False)
		self.HotItem 		= item
		self.SetItemBold(item, True)

		self.view.SetUpEditor ( data[1].address )
		self.view.SetCodePage ( 0 )
		self.view.Request ( self, 'Type in your Model Definition here (make use of the autocomplete for alias/fieldname combos)' )

	def OnPopupAdd(self, event):
		item, data, name = self.ContextPacket

		if data[0]=='Aliases':
			modalias = copy.deepcopy(self.alias_schema)
			self.DialogAlais(data[1], item, 'Add new Alias', modalias)
		elif data[0]=='Associations':
			modlink = copy.deepcopy(self.link_schema)
			aliase  = sorted ( self.GetItemData(self.root)[ data[1] ].aliases.keys() )
			modlink['choicelist']['alias1']['choices']=aliase
			modlink['choicelist']['alias2']['choices']=aliase
			self.DialogAssociation(data[1], item, aliase, 'Add New Association', modlink)
		elif data[0]=='Conditions':
			modcond = copy.deepcopy(self.cond_schema)
			modcond['stc']['expression']['setup'] = self.view.controller.Addresses[data[1]]
			self.DialogCondition(data[1], item, "Add new Condition", modcond)
		elif data[0] in ('ModelGroup','ModelGroupRoot'):
			modgroup = copy.deepcopy(self.modelgroup_schema)
			self.DialogModelGroup( data[1], item, "Add new ModelGroup", modgroup)
		self.Expand(item)
		self.Refresh()

	def OnPopupRemove(self, event):
		item, data, name = self.ContextPacket
		parent = self.GetItemParent(item)
		try:
			if data[0]=='address':
				self.view.controller.DeleteAddress(data[1].name)
			elif data[0]=='alias':
				address = self.GetItemData(parent)[1]
				self.view.controller.DeleteAlias(address, name)
			elif data[0]=='association':
				address = self.GetItemData(parent)[1]
				self.view.controller.DeleteAssociation(address, data[1], data[3], data[2], data[4])
			elif data[0]=='condition':
				address = self.GetItemData(parent)[1]
				self.view.controller.DeleteCondition(address, name)
			elif data[0]=='ModelGroup':
				self.view.controller.DeleteModelGroup(data[2], name)
			elif data[0]=='Model':
				self.view.controller.DeleteModel(name, data[1])
		except AssociationStillInUseException as e:
			self.view.LogMessage( 'Address Bar', 'Could Not Delete Item [%s] - Element may still be referenced' % self.GetItemText(item), statusbar='Delete Association Failed' )
		else:
			self.Delete(item)
			self.Refresh()

class InstanceTreeControl(wx.TreeCtrl):
	def __init__(self, parent, ID, name, view, style=wx.TR_HAS_BUTTONS ):
		wx.TreeCtrl.__init__(self, parent, ID, wx.Point(0, 0), wx.Size(160, 250), style | wx.TR_DEFAULT_STYLE | wx.NO_BORDER)
		imglist = wx.ImageList(16, 16, True, 3)
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_TIP, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_EXECUTABLE_FILE, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_GO_HOME, wx.ART_OTHER, wx.Size(16, 16)))
		imglist.Add(wx.ArtProvider.GetBitmap(wx.ART_WARNING, wx.ART_OTHER, wx.Size(16, 16)))
		self.AssignImageList(imglist)

		self.root	 		= self.AddRoot(name, 2)
		self.view			= view

		#tree events
		self.Bind (wx.EVT_LEFT_DOWN, self.OnLeftDown)
		self.Bind (wx.EVT_RIGHT_DOWN, self.OnRightDown)
		self.Bind(wx.EVT_CONTEXT_MENU, self.OnContextMenu)

		self.instance_schema = 	{
							'title':'Instance Parameters',
							'textfields':{
									'name':{'default':'name of Instance', 'size':(300,-1), 'field':'case sensitive'},
									'mode':{'default':'mode of instance execution', 'size':(300,-1), 'field':'case sensitive'}
									},
							'choicelist':{
								'target address': {'default':None, 'size':(200,-1), 'choices':[], 'populates':'target alias', 'rule':None},
								'target alias': {'default':None, 'size':(200,-1), 'choices':[]},
								'source model': {'default':None, 'size':(200,-1), 'choices':[]}
								}
							}
		self.condition_schema = {
							'title':'Condition Link Parameters',
							'textfields':{
									'scope':{'default':'Scope of Condition', 'size':(300,-1), 'field':'integer value'}
									},
							'choicelist':{
								'address': {'default':None, 'size':(200,-1), 'choices':[], 'populates':'name', 'rule':None},
								'name': {'default':None, 'size':(200,-1), 'choices':[]},
								'model': {'default':None, 'size':(200,-1), 'choices':[]},
								'execution': {'default':'cascade', 'size':(200,-1), 'choices':['cascade','override']}
								}
							}

		self.popupID1 = wx.NewIdRef()
		self.popupID2 = wx.NewIdRef()
		self.popupID3 = wx.NewIdRef()
		self.popupID4 = wx.NewIdRef()
		self.popupID5 = wx.NewIdRef()
		self.popupID6 = wx.NewIdRef()
		self.popupID7 = wx.NewIdRef()
		self.popupID8 = wx.NewIdRef()
		self.popupID9 = wx.NewIdRef()
		self.popupID10 = wx.NewIdRef()

		self.Bind(wx.EVT_MENU, self.OnPopupModifyItem, 		id=self.popupID1)
		self.Bind(wx.EVT_MENU, self.OnPopupMoveUp, 			id=self.popupID2)
		self.Bind(wx.EVT_MENU, self.OnPopupMoveDown, 		id=self.popupID3)
		self.Bind(wx.EVT_MENU, self.OnPopupLinkCondition, 	id=self.popupID4)
		self.Bind(wx.EVT_MENU, self.OnPopupAddInstance, 	id=self.popupID5)
		self.Bind(wx.EVT_MENU, self.OnPopupRunToAddress, 	id=self.popupID6)
		self.Bind(wx.EVT_MENU, self.OnPopupRunToGrid, 		id=self.popupID7)
		self.Bind(wx.EVT_MENU, self.OnPopupDelete, 			id=self.popupID9)
		self.Bind(wx.EVT_MENU, self.OnPopupUnlinkCondtion, 	id=self.popupID10)

	def SetConfigTree(self, InstanceRoot):

		def ProcessInstances(parent_node, instance_link):
			cond_link_node = parent_node
			for condition_link in instance_link.child.conditions:
				cond_link_node = self.AppendItem ( cond_link_node, condition_link.child.name, 3*condition_link.child.type )
				self.SetItemData ( cond_link_node, ('ConditionLink', instance_link, condition_link ) )
			instance_node = self.AppendItem ( cond_link_node, instance_link.child.name, 1 )
			self.SetItemData ( instance_node, ('Instance', instance_link ) )
			for sub_data in instance_link.child.sub_instances:
				ProcessInstances(instance_node, sub_data)

		self.DeleteChildren(self.root)
		if InstanceRoot.child:
			ProcessInstances(self.root, InstanceRoot )

	def OnRightDown(self, event):
		pt = event.GetPosition()
		item, flags = self.HitTest(pt)
		if item:
			self.SelectItem(item)
		event.Skip()

	def OnLeftDown(self, event): #Dnd code here
		pt = event.GetPosition()
		item, flags = self.HitTest(pt)
		if item:
			self.SelectItem(item)
		event.Skip()

	def _getModelObjFromName(self, model_name):
		model = self.view.controller.Addresses[model_name[0]].modelGroupRoot
		for subname in model_name[1:-1]: model = model.children[subname]
		return model.models[ model_name[-1] ]

	def OnContextMenu(self, event):
		self.SetFocus()
		item = self.GetSelection()
		if item:
			self.Expand(item)
			data 		= self.GetItemData(item)
			name 		= self.GetItemText(item)
			menu = wx.Menu()

			if data!=None:
				menu.Append(self.popupID1, "Modify Item")
				if data[0]=='Instance':
					if data[1].instance_node:
						instance_index = data[1].instance_node.sub_instances.index(data[1])
						if instance_index > 0:
							menu.Append(self.popupID2, "Move up")
						if instance_index  < len(data[1].instance_node.sub_instances)-1:
							menu.Append(self.popupID3, "Move down")

					menu.Append(self.popupID4, "Link Condition")
					menu.Append(self.popupID5, "Add Instance")
					sm = wx.Menu()
					sm.Append(self.popupID6, "To Address")
					sm.Append(self.popupID7, "To Grid")
					menu.AppendMenu(self.popupID8, "Run Instance", sm)
					menu.Append(self.popupID9, "Delete Instance")
				elif data[0]=='ConditionLink':
					condition_index = data[1].child.conditions.index(data[2])
					if condition_index>0:
						menu.Append(self.popupID2, "Move up")
					if condition_index<len(data[1].child.conditions)-1:
						menu.Append(self.popupID3, "Move down")
					menu.Append(self.popupID10, "Unlink" )
			else:
				menu.Append(self.popupID5, "Add Root Instance")
				data = ('Instance', None)

			self.ContextPacket = (item, data, name)

			self.Unselect()
			self.PopupMenu(menu)
			menu.Destroy()
			self.SelectItem(item)

	def OnPopupModifyItem(self, event):
		def getModelName(model):
			modelname	= [ model.name ]
			grp			= model.group
			while grp.parent:
				modelname.insert(0, grp.name)
				grp = grp.parent
			return [grp.name] + modelname

		item, data, name 	= self.ContextPacket
		parent 				= self.GetItemParent(item)
		allmodels			= sorted ( self.view.GetModelNames ( strict=True ) )

		if data[0]=='Instance':
			modelname 	= getModelName( data[1].child.model ) if data[1].child.model else []
			modinstance = copy.deepcopy(self.instance_schema)
			modinstance['textfields']['mode']['default'] 			= data[1].child.targetMode
			modinstance['choicelist']['target address']['choices']	= self.view.controller.Addresses.keys()
			modinstance['choicelist']['target address']['rule']		= lambda x:sorted(self.view.controller.Addresses[x].aliases.keys())
			modinstance['choicelist']['target address']['default']	= data[1].child.targetAddress.name
			modinstance['choicelist']['source model']['choices']	= allmodels
			modinstance['choicelist']['source model']['default']	= '.'.join(modelname)
			modinstance['choicelist']['target alias']['default']	= data[1].child.targetAlias_name
			del modinstance['textfields']['name']
			self.DialogInstance ( data[1], item, 'Modify Instance', modinstance, key=name )

		elif data[0]=='ConditionLink':
			modelname 	= getModelName( data[2].model ) if data[2].model else []
			modcond 	= copy.deepcopy(self.condition_schema)
			modcond['textfields']['scope']['default'] = str(data[2].scope)
			modcond['choicelist']['model']['choices'] = [x for x in allmodels if x.startswith(data[2].child.address_name)]
			modcond['choicelist']['model']['default'] = '.'.join(modelname)
			modcond['choicelist']['execution']['default'] = 'override' if data[2].override else 'cascade'
			del modcond['choicelist']['address']
			del modcond['choicelist']['name']
			self.DialogConditionLink(data[1], item, "Modify ConditionLink", modcond, key=data[2] )

		self.Expand(item)

	def OnPopupMoveUp(self, event):
		item, data, name = self.ContextPacket
		if data[0]=='Instance':
			move(data[1].instance_node.sub_instances, data[1], up=True)
		elif data[0]=='ConditionLink':
			move(data[1].child.conditions, data[2], up=True)
		self.SetConfigTree ( self.view.controller.GetInstanceRoot() )
		self.ExpandAllChildren(self.root)
		self.Refresh()

	def OnPopupMoveDown(self, event):
		item, data, name = self.ContextPacket
		if data[0]=='Instance':
			move(data[1].instance_node.sub_instances, data[1], up=False)
		elif data[0]=='ConditionLink':
			move(data[1].child.conditions, data[2], up=False)
		self.SetConfigTree(self.view.controller.GetInstanceRoot() )
		self.ExpandAllChildren(self.root)
		self.Refresh()

	def OnPopupLinkCondition(self, event):
		item, data, name = self.ContextPacket
		allmodels		 = sorted ( self.view.GetModelNames ( strict=True ) )
		modcond 		 = copy.deepcopy(self.condition_schema)
		modcond['choicelist']['address']['choices']	= self.view.controller.Addresses.keys()
		modcond['choicelist']['model']['choices']	= allmodels
		modcond['choicelist']['address']['rule']	= lambda x:sorted(self.view.controller.Addresses[x].conditions.keys())
		self.DialogConditionLink(data[1], item, 'Link New Condition', modcond)

	def OnPopupUnlinkCondtion(self, event):
		item, data, name = self.ContextPacket
		self.view.controller.UnlinkConditionFromInstance(data[2].child, data[1].child)
		self.SetConfigTree(self.view.controller.GetInstanceRoot() )
		self.ExpandAllChildren(self.root)
		self.Refresh()

	def DialogConditionLink(self, parent_link, item, title, schema, key = None):
		dlg = TextDialog(self, -1, title, schema, size=(400, 300), style=wx.DEFAULT_DIALOG_STYLE, verticalChoices=True)
		dlg.CenterOnScreen()
		if dlg.ShowModal()==wx.ID_OK:
			textvalues 		= dlg.GetValues('textfields')
			choicevalues 	= dlg.GetValues('choicelist')
			condition 		= key.child if key else self.view.controller.Addresses[ choicevalues['address'] ].conditions[ choicevalues['name'] ]
			model 			= self._getModelObjFromName( choicevalues['model'].split('.') ) if choicevalues['model'] else None

			condition_link	= self.view.controller.LinkConditionToInstance ( condition, parent_link.child, int( textvalues['scope'] ), model, choicevalues['execution']=='override' )

			if not key:
				self.SetConfigTree(self.view.controller.GetInstanceRoot())
				self.ExpandAllChildren(self.root)
				self.Refresh()
			else:
				self.SetItemData ( item, ('ConditionLink', parent_link, condition_link ) )

		dlg.Destroy()

	def DialogInstance(self, parent_link, item, title, schema, key = None):
		dlg = TextDialog(self, -1, title, schema, size=(640, 480), style=wx.DEFAULT_DIALOG_STYLE, verticalChoices=True )
		dlg.CenterOnScreen()
		if dlg.ShowModal()==wx.ID_OK:
			textvalues 		= dlg.GetValues('textfields')
			choicevalues 	= dlg.GetValues('choicelist')
			model_name 		= choicevalues['source model'].split('.')
			model 			= self._getModelObjFromName( model_name ) if choicevalues['source model'] else None

			self.view.controller.AddInstance ( getattr(parent_link, 'instance_node' if key else 'child' ) if parent_link else None, textvalues['name'] if not key else key, choicevalues['target address'], choicevalues['target alias'], textvalues['mode'], True, model )

			if not key:
				self.SetConfigTree(self.view.controller.GetInstanceRoot())
				self.ExpandAllChildren(self.root)
				self.Refresh()

		dlg.Destroy()

	def OnPopupAddInstance(self, event):
		item, data, name = self.ContextPacket
		modinstance 	 = copy.deepcopy(self.instance_schema)
		allmodels		 = sorted ( self.view.GetModelNames ( strict=True ) )
		modinstance['choicelist']['target address']['choices']	= self.view.controller.Addresses.keys()
		modinstance['choicelist']['target address']['rule']		= lambda x:sorted(self.view.controller.Addresses[x].aliases.keys())
		modinstance['choicelist']['source model']['choices']	= allmodels
		self.DialogInstance(data[1], item, 'Add New Instance', modinstance)

	def OnPopupDelete(self, event):
		item, data, name = self.ContextPacket
		self.view.controller.DeleteInstance(data[1].parent, name)
		self.SetConfigTree(self.view.controller.GetInstanceRoot())
		self.ExpandAllChildren(self.root)
		self.Refresh()

	def OnPopupRunToAddress(self, event):
		item, data, name = self.ContextPacket
		self.view.LogMessage('Instance', 'About to execute Instance - May take a while')
		try:
			runlog = data[1].child.Execute([])
		except:
			wx.Bell()
			self.view.LogMessage('Instance', 'UnHandled Exception', statusbar='UnHandled Exception')
			self.view.LogTraceBack(GE_Editor.FunctionError(sys.exc_info()))
		else:
			self.view.LogMessage('Execution Sucessful', 'Started [%s] Ended [%s] Instance data written to address' % (runlog.start.strftime('%Y-%m-%d %H:%M:%S'), runlog.end.strftime('%Y-%m-%d %H:%M:%S')) , statusbar='ETL sucessful')

	def OnPopupRunToGrid(self, event):
		item, data, name = self.ContextPacket
		self.view.LogMessage('Instance', 'This feature is currently disabled - We appologise for the inconveniance (not really)')

class MyNoteBook(wx.Notebook):
	def __init__(self, parent, id):
		wx.Notebook.__init__( self, parent, id, size=wx.DefaultSize, style=wx.NB_TOP| wx.BORDER_SUNKEN )

		self.grid	 = gridlib.Grid(self, wx.NewIdRef())
		self.output  = OutputTable()

		#The grid owns the table
		self.grid.SetTable(self.output, True)
		self.AddPage(self.grid, "Output")

		self.messages = wx.TextCtrl(self, wx.NewIdRef(), "[Synapse Messages]\n", size=wx.DefaultSize, style=wx.TE_MULTILINE)
		self.messages.SetInsertionPoint(0)
		self.AddPage(self.messages, "Messages")

		self.ErrorPages = {}

	def RemoveErrorPage(self, name):
		if name in self.ErrorPages:
			index, oldpanel = self.ErrorPages[name]
			oldpanel.Destroy()
			self.RemovePage(index)
			del self.ErrorPages[name]
			self.Refresh()

	def AddErrorPage(self, name, panel):
		self.RemoveErrorPage(name)
		self.AddPage(panel, 'Error: %s'%name)
		for page in range(self.GetPageCount()):
			if self.GetPageText(page)=='Error: %s'%name:
				self.ErrorPages[name]=(page, panel)
				self.SetSelection(page)
		self.Refresh()

class MyFrame(wx.Frame):
	def __init__(self, parent, ID, title, environment, pos=wx.DefaultPosition,
				 size=wx.DefaultSize, session=None, style=wx.DEFAULT_FRAME_STYLE):

		wx.Frame.__init__(self, parent, ID, title, pos, size, style)

		self._mgr = aui.AuiManager()
		self._mgr.SetManagedWindow(self)
		self.SetIcon(images.Mondrian.GetIcon())
		self._notebook_style = aui.AUI_NB_DEFAULT_STYLE | aui.AUI_NB_TAB_EXTERNAL_MOVE | wx.NO_BORDER
		self._notebook_theme = 0
		self._textCount = 1
		self._transparency = 255
		self._snapped = False
		self._custom_pane_buttons = False
		self._custom_tab_buttons = False
		self._pane_icons = False

		self.KeyState			= None
		self.environment		= environment
		self.ModelFns			= ModelFunctionCatalog( self.environment )
		self.InstanceFns		= InstanceFunctionCatalog ( self.environment )
		self.envKeyWords 		= [x for x in dir(self.environment) if not inspect.ismethod(getattr(self.environment, x)) and not x.startswith('__') ]
		self.codePage   		= {}
		self.FunctionModules	= {}
		self.session 			= session
		self.controller			= None

		self.environment.SetLogger(self.LogMessage)
		currenttime = time.localtime()
		self.environment.Rundate =  '%s/%s/%s' % (currenttime.tm_year, currenttime.tm_mon, currenttime.tm_mday)
		self.MakeStatusbar()
		self.SetupBinds()
		self.MakeMenuBar()
		self.Load()
		self.BindEvents()

	def BindEvents(self):
		self.Bind(wx.EVT_MENU, self.NewSession, id=self.binds['NewInstance'])
		self.Bind(wx.EVT_MENU, self.OnUndo, id=self.binds['Undo'])
		self.Bind(wx.EVT_MENU, self.OnSave, id=self.binds['SaveInstance'])
		self.Bind(wx.EVT_MENU, self.LoadSession, id=self.binds['LoadInstance'])
		self.Bind(wx.EVT_MENU, self.OnCloseWindow, id=wx.ID_EXIT)
		self.Bind(wx.EVT_MENU, self.OnAbout, id=wx.ID_ABOUT)
		self.Bind(wx.EVT_CLOSE, self.OnCloseWindow)
		self.Bind(wx.adv.EVT_DATE_CHANGED, self.OnChangeDate)

	def SetupBinds(self):
		self.binds = {'NewInstance'			:wx.NewIdRef(),
					  'SaveInstance'		:wx.NewIdRef(),
					  'LoadInstance'		:wx.NewIdRef(),
					  'Undo'				:wx.NewIdRef(),
					  'CreatePerspective'	:wx.NewIdRef(),
					  'CopyPerspectiveCode'	:wx.NewIdRef(),
					  'FirstPerspective'	:wx.NewIdRef(),
					  'AllPerspective'		:wx.NewIdRef()
					  }
		
	def MakeMenuBar(self):
		mb = wx.MenuBar()

		file_menu = wx.Menu()
		file_menu.Append(self.binds['NewInstance'], "New Session")
		file_menu.Append(self.binds['SaveInstance'], "Save Session")
		file_menu.Append(self.binds['LoadInstance'], "Load Session")
		file_menu.Append(wx.ID_EXIT, "Exit")
		
		perspectives_menu = wx.Menu()
		
		perspectives_menu.Append(self.binds['CreatePerspective'], "Create Perspective" )
		perspectives_menu.Append(self.binds['CopyPerspectiveCode'], "Copy Perspective Data To Clipboard" )
		perspectives_menu.AppendSeparator()
		perspectives_menu.Append(self.binds['FirstPerspective'], "Default Startup" )
		perspectives_menu.Append(self.binds['AllPerspective'], "All Panes" )
		
		help_menu = wx.Menu()
		help_menu.Append(wx.ID_ABOUT, "About...")

		mb.Append(file_menu, "&File")		
		mb.Append(perspectives_menu, "&Perspectives")		
		mb.Append(help_menu, "&Help")
		
		self.SetMenuBar(mb)
		
		mb.GetMenu(0).Enable(self.binds['SaveInstance'], False)
	
	def MakeStatusbar(self):
		self.sb = CustomStatusBar(self)
		self.SetStatusBar(self.sb)

	def LogTraceBack(self, error):
		self.LogMessage('Synapse', '*****************************')
		self.LogMessage('Synapse', 'Exception Occured - Traceback')
		self.LogMessage('Synapse', '*****************************')
		self.LogMessage('Synapse', '')
		for trace in error.traceback:			
			self.LogMessage('"%s" - line %d - "%s" ' % trace[:3], trace[3])
		self.LogMessage('"%s" - line %d - "%s" ' % error.traceback[-1][:3], '%s - %s' % (error.exception_type, error.exception_details))

	def SetCodePage(self, num):
		self.codebook.SetSelection(num)
		
	def LogMessage(self, source, text, statusbar=None):
		self.notebook.SetSelection(1)
		self.notebook.messages.AppendText('\n[%s] %s' % (source, text) )
		if statusbar:
			self.sb.setText( statusbar )
	
	def GetOutputWindow(self):
		#assume we want to look at the grid whenever somebody asks for the output window
		self.notebook.SetSelection(0)
		return self.notebook.output
	
	def GetGridWindow(self):
		return self.notebook.grid
	
	def OnAbout(self, event):
		wx.MessageBox("Synapse Demo\nA simple dynamic data manipulation framework",
					  "About Synapse Demo", wx.OK, self)

	#code specific crap
	def LoadCatalogue(self, catalogueName, modulename):
		try:
			wx.BeginBusyCursor()
			self.Freeze()
			if os.path.exists(GE_Editor.GetOriginalFilename(catalogueName)):
				self.FunctionModules[catalogueName] = GE_Editor.FunctionModules(catalogueName)
				self.codePage[catalogueName] 		= GE_Editor.CodePanel(self.codebook, self)
				self.codePage[catalogueName].LoadFunction(self.FunctionModules[catalogueName], modulename)
		finally:
			wx.EndBusyCursor()
			self.Thaw()
			
	def RunModule(self, catalogueName, modulename):
		"""Runs the active module"""
		module = self.FunctionModules[catalogueName].GetActive()
		updateFunctionMap = {'ModelFunctionCatalog':self.ModelFns}
		DataObj = None
		#There's a bug here - if the module is none, you can't double click it in the error panel
		if module is not None:
			try:
				DataObj = getattr(module,modulename)(self.environment)
			except:				
				self.notebook.AddErrorPage( catalogueName, GE_Editor.FunctionErrorPanel(self.notebook, self.codePage[catalogueName], GE_Editor.FunctionError(sys.exc_info())) )
			else:
				updateFunctionMap [modulename]=DataObj
				self.notebook.RemoveErrorPage(catalogueName)
		else:
			# There was a previous error in compiling or exec-ing
			self.notebook.AddErrorPage( catalogueName, GE_Editor.FunctionErrorPanel(self.notebook, self.codePage[catalogueName], self.FunctionModules[catalogueName].GetErrorInfo()) )

	def UpdateDatabases(self):
		self.sqleditor.Refresh()
		
	def MakePanes(self):
		self.instancetree = InstanceTreeControl ( self, -1, self.session, self)
		self.instancetree.SetConfigTree(self.controller.GetInstanceRoot())
		
		self.addresstree = AddressTreeControl( self, -1, 'Addresses', self )
		self.addresstree.SetConfigTree(self.controller.Addresses)
		self.addresstree.Expand(self.addresstree.root)

		self.notebook		= MyNoteBook(self, -1)
		self.codebook		= wx.Notebook(self, -1, size=wx.DefaultSize, style=wx.NB_TOP)
		self.master			= MyModelEditor(self.codebook, -1, style=wx.RAISED_BORDER, statusBar=self.sb, myView=self)
		self.master.SetText(DefaultEditorText)		
		self.sqleditor 		= MySQLEditor(self.codebook, wx.NewIdRef(), self)
		
		self.codebook.AddPage(self.master, 'Model Editor', imageId=0)
		self.LoadCatalogue('GE_DataFunctions', 'ModelFunctionCatalog')
		for page, value in self.codePage.items():
			self.codebook.AddPage(value, page)
		
		self.codebook.AddPage( self.sqleditor, 'SQL Explorer')
		
		tb = aui.AuiToolBar(self, -1, wx.DefaultPosition, wx.DefaultSize, aui.AUI_TB_DEFAULT_STYLE | aui.AUI_TB_OVERFLOW)
		
		self.rundate = wx.adv.DatePickerCtrl(tb, size=wx.DefaultSize, style=wx.adv.DP_DROPDOWN | wx.adv.DP_SHOWCENTURY)
		self.environment.Rundate = str(self.rundate.GetValue().Format('%Y/%m/%d'))
		
		tb.SetToolBitmapSize(wx.Size(16, 16))
		tb.AddSimpleTool(self.binds['LoadInstance'], "Load", wx.ArtProvider.GetBitmap(wx.ART_FILE_OPEN, wx.ART_OTHER, wx.Size(16, 16)))
		tb.AddSimpleTool(self.binds['SaveInstance'], "Save", wx.ArtProvider.GetBitmap(wx.ART_FILE_SAVE, wx.ART_OTHER, wx.Size(16, 16)))
		tb.AddSimpleTool(self.binds['Undo'], "Undo", wx.ArtProvider.GetBitmap(wx.ART_UNDO, wx.ART_OTHER, wx.Size(16, 16)))
		tb.AddSimpleTool(wx.ID_EXIT, "Exit", wx.ArtProvider.GetBitmap(wx.ART_QUIT, wx.ART_OTHER, wx.Size(16, 16)))
		tb.AddSeparator()
		tb.AddControl(wx.StaticText(tb, -1, "Rundate"))
		tb.AddControl(self.rundate, label='Rundate')
		tb.Realize()
		
		self._mgr.AddPane(self.instancetree, aui.AuiPaneInfo().Name("instancetree").Caption("Instance Pane").
						  Left().Layer(1).Position(1).CloseButton(False).MaximizeButton(True).MinimizeButton(True))
		
		self._mgr.AddPane(self.addresstree, aui.AuiPaneInfo().Name("addresstree").Caption("Address Pane").
						  Right().Layer(1).Position(1).CloseButton(False).MaximizeButton(True).
						  MinimizeButton(True))

		self._mgr.AddPane(self.codebook, aui.AuiPaneInfo().Name("codebook").CenterPane().MinimizeButton(True))

		self._mgr.AddPane(self.notebook, aui.AuiPaneInfo().Name("notebook").Caption("Log Area").
						  Bottom().MinSize(wx.Size(200,150)).Layer(1).Position(1).MaximizeButton(True).MinimizeButton(True))
		
		self._mgr.AddPane(tb, aui.AuiPaneInfo().Name("toolbar").Caption("Toolbar").ToolbarPane().Top().Row(1))
		
		perspective_all = self._mgr.SavePerspective()
		self._mgr.Update()

	def Request( self, origin, text, readonly=False):
		if readonly:
			self.master.SetReadOnly(False)
			self.master.SetText(text)
			self.master.SetReadOnly(True)
		else:	
			self.master.Request( origin, text )

	def GetModelNames (self, strict=True):
		def update_groups(modelset, current_group, namesofar):
			for model_name in current_group.models.keys():
				if not strict:
					modelset.add( model_name )
					modelset.add( '.'.join(namesofar[1:]+[model_name]) )
				modelset.add( '.'.join(namesofar+[model_name]) )
				
			for child_group, child_group_data in current_group.children.items():
				update_groups(modelset, child_group_data, namesofar+[child_group] )

		models = set()
		for address in self.controller.Addresses.values():
			update_groups( models, address.modelGroupRoot, [address.name])
		return models
				
	def SetUpEditor(self, address, editor=None):
					
		fields = set()
		if address.address_type!='Directory':
			for table in address.schema.meta.sorted_tables:
				for col in table.columns:
					for alias in [x for x, d in address.aliases.items() if d.resource_name == table.name]:
						fields.add('%s.%s' % (alias, col.name))
		
		if editor:
			editor.SetKeyWords(2, ' '.join(fields))
			editor.hotkeys = sorted(fields)
		else:
			hotkeys = list( fields.union ( self.GetModelNames ( strict=False ) ) )
			self.master.SetKeyWords (2, ' '.join( hotkeys ) )
			self.master.hotkeys = sorted(hotkeys)
			
	def Load(self):
		if self.session:
			self.controller = Context(self.environment, self.ModelFns, self.InstanceFns)
			self.controller.LoadSystem(self.session)
			self.MakePanes()
			self.GetMenuBar().GetMenu(0).Enable(self.binds['SaveInstance'], True)

	def NewSession(self, event):
		newsession = None
		dlg = wx.TextEntryDialog(self, 'Please enter the correct DB URL for a new Synapse instance', 'Enter New DB URL', 'sqlite:///')
		if dlg.ShowModal() == wx.ID_OK:
			newsession = dlg.GetValue()
			setup(newsession)
		dlg.Destroy()
		self.session = newsession
		self.Load()

	def LoadSession(self, event):
		session = None
		dlg = wx.TextEntryDialog(self, 'Please enter the correct DB URL for the Synapse instance', 'Enter Existing DB URL', 'sqlite:///')
		if dlg.ShowModal() == wx.ID_OK:
			session = dlg.GetValue()
		dlg.Destroy()
		self.session = session
		self.Load()
		
	def OnUndo(self, event):
		self.controller.Undo()
		self.instancetree.SetConfigTree(self.controller.GetInstanceRoot())
		self.addresstree.SetConfigTree(self.controller.Addresses)
		self.ReDraw(event)
		self.LogMessage('Session', 'Session Rolled back to last save-point', statusbar='Session Undone')
				
	def ReDraw(self, event):
		if event:			
			h,w = self.GetSize()
			self.SetSize((h+1, w))
			self.SetSize((h, w))
		self.Refresh()
		
	def OnSave(self, event):
		self.controller.Save()
		self.LogMessage('Session', 'Session Saved', statusbar='Session Saved')
					
	def OnChangeDate(self, event):
		newDate = event.GetDate()
		if newDate.Format('%Y/%m/%d') != self.environment.Rundate:
			self.environment.SetRunDate(str(newDate.Format('%Y/%m/%d')))
			self.LogMessage('Rundate','Rundate set to %s' % self.environment.Rundate, statusbar='Rundate changed')
		event.Skip()
		
	def OnCloseWindow(self, event):
		if self.controller and self.controller.NeedsSaving():
			dlg = wx.MessageDialog(self, 'Save the session?', 'Synapse', wx.YES_NO )
			result=dlg.ShowModal()
			if result==wx.ID_YES:
				self.OnSave(None)
			dlg.Destroy()
		if self._mgr: self._mgr.UnInit()
		self.Destroy()

class AppLauncher(wx.App):
	def OnInit(self):
		#parse command line arguments
		parser = OptionParser()
		parser.add_option("-s", "--session", dest="session", help="read session from dsn")
		(options, args) = parser.parse_args()
		
		environment = Environment()
		provider = wx.SimpleHelpProvider()
		wx.HelpProvider.Set(provider)
		frame = MyFrame(None, -1, __version__, environment, pos=wx.DefaultPosition,
						size=(800, 600), session=options.session,
						style=(wx.DEFAULT_FRAME_STYLE|wx.FULL_REPAINT_ON_RESIZE|wx.SUNKEN_BORDER))
		
		frame.CenterOnScreen()
		frame.Show()
		return True
	
if __name__ == '__main__':
	launcher = AppLauncher(redirect=1, filename='synapse_eventlog.txt')
	launcher.MainLoop()
