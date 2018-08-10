package com.allinfinance.tools.param.util

import scala.swing._
import scala.swing.event._
import Swing._
import org.apache.poi.ss.usermodel.{Sheet, Row, Cell}
import org.apache.poi.hssf.usermodel._
import java.io.FileInputStream
import scala.collection.JavaConversions._

import ObjTreeNode.{VerConfigs, VerNode, MyWorker}

class ParamExportDialog(org: String, verNode: VerNode, parent: Window=null) extends Dialog(parent) {
    var task: javax.swing.SwingWorker[Unit, String] = null
    val textArea = new TextArea
    val scrollTextArea = new ScrollPane(textArea) {
        preferredSize = new java.awt.Dimension(800, 300)
        horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
        verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    }
    val progressBar = new ProgressBar {
        labelPainted = true
        visible = false
    }
    val btnExport = new Button

    class ExportTask(path: String, org: String, verNode: VerNode, sheetDescs: Seq[ParamExport.SheetDesc], button: Button) extends javax.swing.SwingWorker[Unit, String] {
        addPropertyChangeListener(new java.beans.PropertyChangeListener {
            override def propertyChange(evt: java.beans.PropertyChangeEvent) {
                if (evt.getPropertyName == "progress") {
                    val i = evt.getNewValue.asInstanceOf[java.lang.Integer]
                    progressBar.value = i
                }
            }
        })
        override def doInBackground: Unit = {
            try {
                setProgress(0)
                ParamExport.export(path, org, verNode, sheetDescs) {
                    case (sheetDesc,idx) =>
                        if (isCancelled) throw new IllegalArgumentException("cancelled")
                        publish("export " + sheetDesc.name)
                        setProgress(math.min(idx*100/sheetDescs.size, 100))
                }
            }
            catch {case e: Throwable => e.printStackTrace}
        }
        override def process(logs:java.util.List[String]) {
            logs.toList.foreach (x => textArea.text += x + "\n")
        }
        override def done {
            button.enabled = true
            progressBar.visible = false
        }
    }

    class SheetSelectModel extends javax.swing.table.AbstractTableModel {
        var rowData = Vector[Vector[Any]]()
        val columnNames = Vector("Class Name", "desc", "Is List")
        val columnClazz = Vector("".getClass, "".getClass, false.getClass)

        override def getColumnName(column: Int) = columnNames(column).toString
//        override def getColumnClass(column: Int) = columnClazz(column)
        def getRowCount() = rowData.length
        def getColumnCount() = columnNames.length
        def getValueAt(row: Int, col: Int): AnyRef = rowData(row)(col).asInstanceOf[AnyRef]
        override def isCellEditable(row: Int, column: Int) = true
        override def setValueAt(value: Any, row: Int, col: Int) {
            val v = rowData(row)
            rowData = (rowData.take(row) :+ ((v.take(col) :+ value) ++ v.drop(col+1))) ++ rowData.drop(row+1)
            fireTableCellUpdated(row, col)
        }
        def addRow(clazz: String) {
            rowData = rowData :+ Vector(clazz, clazz, false)
            fireTableRowsInserted(rowData.size-1, rowData.size-1)
        }
        def removeRow(row: Int) {
            rowData = rowData.take(row) ++ rowData.drop(row+1)
            fireTableRowsDeleted(row, row)
        }
        def removeRows(rows: Seq[Seq[Any]]) {
            rowData = rowData diff rows
            fireTableStructureChanged()
        }
    }

    class SheetSelectView extends ScrollPane {
        val table = new Table {
            model = new SheetSelectModel
        }
        contents = table

//        preferredSize = new java.awt.Dimension(500,200)
        horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
        verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded

    }
    var allClasses = verNode.getClasses
    val clazzListView = new ListView(allClasses)
    var tabComponent: java.awt.Component = null
    val tabbedPane = new TabbedPane

    val inplaceEditor = new TextField {border = null}
    def cancelEdit(): Unit = {
        tabbedPane.peer.setTabComponentAt(tabbedPane.selection.index, tabComponent)
        tabComponent = null
        inplaceEditor.visible = false
        tabbedPane.peer.requestFocusInWindow
    }
    def activePage = tabbedPane.selection.page.content.asInstanceOf[SheetSelectView]

    contents = new BoxPanel(Orientation.Vertical) {
        contents += new BoxPanel(Orientation.Horizontal) {
            contents += new Label("org:")
            contents += new TextField(org)
            contents += new Label(" ")
            contents += new Label("version:")
            contents += new TextField(verNode.toString)
        }
        val left = new BoxPanel(Orientation.Vertical) {
            contents += new Label("export sheets")
            tabbedPane.pages += new TabbedPane.Page("+", new Label)
            contents += tabbedPane

            listenTo(tabbedPane.selection)
            listenTo(tabbedPane.mouse.clicks)
            listenTo(inplaceEditor.keys)
            reactions += {
//                case SelectionChanged(_) => println("tab changed")
                case _: MousePressed =>
                    cancelEdit()
                case mc: MouseClicked =>
                    val rectPlus = tabbedPane.peer.getUI.getTabBounds(tabbedPane.peer, tabbedPane.pages.size-1)
                    if ((rectPlus != null) && rectPlus.contains(mc.point)) {
                        // last page "+"
                        val idx = tabbedPane.pages.size-1
                        tabbedPane.pages.insert(idx, new TabbedPane.Page("sheet" + (tabbedPane.pages.size-1), new SheetSelectView))
                        tabbedPane.selection.index = idx
                        mc.consume
                    }
                    else { // check double click
                        val rect = tabbedPane.peer.getUI.getTabBounds(tabbedPane.peer, tabbedPane.selection.index)
                        if ((rect != null) && rect.contains(mc.point) && (mc.clicks == 2)) {
                            tabComponent = tabbedPane.peer.getTabComponentAt(tabbedPane.selection.index)
                            tabbedPane.peer.setTabComponentAt(tabbedPane.selection.index, inplaceEditor.peer)
                            inplaceEditor.text = tabbedPane.selection.page.title
                            inplaceEditor.selectAll
                            inplaceEditor.visible = true
                            inplaceEditor.requestFocus
                        }
                    }
//                case EditDone(_) =>
                case KeyPressed(_, Key.Enter, _, _) =>
                    // inplaceEditor.peer.stopCellEditing
                    tabbedPane.selection.page.title = inplaceEditor.text.trim
                    cancelEdit()
                    println("edit none")
                case _: FocusLost =>
                    println("focus lost")
                    cancelEdit()
                case KeyPressed(_, Key.Escape, _, _) =>
                    cancelEdit()
            }

        }
        val right = new BoxPanel(Orientation.Vertical) {
            contents += new Label("avalible classes")
            contents += new ScrollPane(clazzListView) {
//                preferredSize = new java.awt.Dimension(600,600)
                horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
                verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
            }
        }
        contents += new BoxPanel(Orientation.Horizontal) {
            contents += new SplitPane(Orientation.Vertical, left, right) {
                continuousLayout = true
            }
        }
        contents += new BoxPanel(Orientation.Vertical) {
            contents += new Label("logs:")
            contents += scrollTextArea
        }
        contents += new BoxPanel(Orientation.Horizontal) {
            contents += new Button(Action(">>>") {
                clazzListView.listData = (clazzListView.listData ++ activePage.table.selection.rows.map {row => activePage.table.model.getValueAt(row,0).asInstanceOf[String]}).toList.sorted
                activePage.table.selection.rows.toList.sortWith {_ > _} foreach {
                    row => activePage.table.model.asInstanceOf[SheetSelectModel].removeRow(row)
                }
            })
            contents += new Button(Action("<<<") {
                clazzListView.selection.items foreach {
                    x => activePage.table.model.asInstanceOf[SheetSelectModel].addRow(x)
                }
                clazzListView.listData = clazzListView.listData.diff(clazzListView.selection.items)
            })
            contents += new Label(" ")
            contents += btnExport
            btnExport.action = Action("Export") {
                val chooser = new FileChooser {
                    peer.setCurrentDirectory(new java.io.File(System.getProperty("user.dir")))
                }
                chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
                if (chooser.showOpenDialog(this) == FileChooser.Result.Approve) {
                    val sheetDescs = tabbedPane.pages.init.map {
                        page =>
                            val clazzes = page.content.asInstanceOf[SheetSelectView].table.model.asInstanceOf[SheetSelectModel].rowData.map {
                                row => (row(0).asInstanceOf[String], row(1).asInstanceOf[String], row(2).asInstanceOf[Boolean])
                            }
                            clazzes.size match {
                                case 0 => None
                                case _ => Some(ParamExport.SheetDesc(page.title, clazzes))
                            }
                    }.filter(!_.isEmpty).map {_.get}
                    if (sheetDescs.size > 0) {
                        progressBar.visible = true
                        btnExport.enabled = false
                        task = new ExportTask(chooser.selectedFile.getPath, org, verNode, sheetDescs, btnExport)
                        task.execute
                    }
                }
            }
        }
        contents += progressBar
    }

    title = "export params dialog"
    centerOnScreen()
    modal = true
    open()
}

