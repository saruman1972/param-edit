package com.allinfinance.tools.param.util

import scala.swing._
import scala.swing.event._
import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.JTable
import javax.swing.table.AbstractTableModel
import javax.swing.DropMode
import javax.swing.JComboBox
import javax.swing.DefaultCellEditor

import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.StringSelection
import java.awt.datatransfer.Transferable
import javax.swing.TransferHandler

class ObjContentView extends ScrollPane {
    val table = new Table {
        rowHeight = 25
        autoResizeMode = Table.AutoResizeMode.Off
        showGrid = true
        gridColor = new java.awt.Color(150, 150, 150)

        model = new AbstractTableModel {
          override def getColumnName(column: Int) = column match {
            case 0 => "sunday"
            case 1 => "monday"
            case 2 => "tuseday"
            case 3 => "wendsday"
            case 4 => "thursday"
            case 5 => "friday"
            case 6 => "satuarday"
            case _ => "..."
          }
          override def getRowCount() = 10
          override def getColumnCount() = 10
          override def getValueAt(row: Int, column: Int): AnyRef = column match {
            case 1 => true: java.lang.Boolean
            case _ => "abcdef"
          }
          override def isCellEditable(row: Int, column: Int) = true
          override def setValueAt(value: Any, row: Int, column: Int) {
            fireTableCellUpdated(row, column)
          }
        }

        val column = peer.getColumnModel().getColumn(0)
        val comboBox = new JComboBox[String]()
        comboBox.addItem("aaaaa")
        comboBox.addItem("bbbbb")
        comboBox.addItem("ccccccccc")
        comboBox.addItem("dddddddddddd")
        comboBox.addItem("eeee")
        val comboBoxEditor = new DefaultCellEditor(comboBox)
        column.setCellEditor(comboBoxEditor)
 
        override def editor(row: Int, column: Int) = {
          if (column == 0)
            comboBoxEditor
          else
            super.editor(row, column)
        }
    }

    val rowHeader = new ListView((0 until table.rowHeight) map (_.toString)) {
        fixedCellWidth = 30
        fixedCellHeight = table.rowHeight
    }

    viewportView = table
    rowHeaderView = rowHeader
}

