package com.allinfinance.tools.param.util

import scala.swing._
import Swing._
import org.apache.poi.ss.usermodel.{Sheet, Row, Cell}
import org.apache.poi.hssf.usermodel._
import java.io.FileInputStream
import scala.collection.JavaConversions._

import ObjTreeNode.{VerConfigs, VerNode, MyWorker}

class ErrorDialog(label: String, msg: String, parent: Window=null) extends Dialog(parent) {
    contents = new BoxPanel(Orientation.Vertical) {
        contents += new Label("error while loading:")
        contents += new ScrollPane(new TextArea(msg)) {
            preferredSize = new java.awt.Dimension(600,600)
            horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
            verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
        }
        contents += new Button("OK")
    }

    title = label
    centerOnScreen()
    modal = true
    open()
}

class ParamLoadDialog(label: String, genLoadStmts: (String,String,String) => Either[String,List[String]], parent: Window=null) extends Dialog(parent) {
    var task: javax.swing.SwingWorker[Unit, String] = null
    val textArea = new TextArea
    val scrollTextArea = new ScrollPane(textArea) {
        preferredSize = new java.awt.Dimension(800, 300)
        horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
        verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    }
    val executeAllBtn = new Button
    executeAllBtn.enabled = false
    val executeSelectedBtn = new Button
    executeSelectedBtn.enabled = false
    val progressBar = new ProgressBar {
        labelPainted = true
        visible = false
    }
    class LoadTask(message: String, sqls: Seq[String], button: Button) extends javax.swing.SwingWorker[Unit, String] {
        import Parser.CmdParser
        val parser = new CmdParser
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
                sqls.zipWithIndex.foreach {
                    case (sql,idx) =>
                        if (isCancelled) throw new IllegalArgumentException("cancelled")
                        val result = parser.parseAll(parser.cmd, sql)
                        publish(sql)
                        result match {
                            case parser.Success(p, _) => p match {
                                case _ =>
                                    p.resolveType
                                    publish(p.eval.toString)
                            }
                            case f: parser.NoSuccess =>
                                publish(f.toString)
                        }
                        setProgress(math.min(idx*100/sqls.size, 100))
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
            task = null
        }
    }
    executeAllBtn.action = Action("execute all") {
        task = new LoadTask("executing sqls", sqlListView.listData, executeAllBtn)
        task.execute
        executeAllBtn.enabled = false
        progressBar.visible = true
    }
    executeSelectedBtn.action = Action("execute selected") {
        task = new LoadTask("executing sqls", sqlListView.selection.items, executeSelectedBtn)
        task.execute
        executeSelectedBtn.enabled = false
        progressBar.visible = true
    }

    val sqlListView = new ListView(List.empty[String])
    val scrollListView = new ScrollPane(sqlListView) {
        preferredSize = new java.awt.Dimension(600, 300)
        horizontalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
        verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
    }

    contents = new BoxPanel(Orientation.Vertical) {
        val orgText = new TextField
        orgText.peer.setColumns(12)
        val versionList = new ComboBox(VerConfigs.nodes)
        contents += new BoxPanel(Orientation.Horizontal) {
            contents += new Label("org:")
            contents += orgText
            contents += new Label("version:")
            contents += versionList
        }
        contents += new Label("sqls to be executed:")
        contents += scrollListView
        contents += new Label("executing logs:")
        contents += scrollTextArea
        contents += new BoxPanel(Orientation.Horizontal) {
            contents += new Button(Action("load") {
                val chooser = new FileChooser {
                    peer.setCurrentDirectory(new java.io.File(System.getProperty("user.dir")))
                }
                chooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
                if (chooser.showOpenDialog(this) == FileChooser.Result.Approve) {
                    genLoadStmts(chooser.selectedFile.getPath, orgText.text, versionList.selection.item.toString) match {
                        case Left(err) => new ErrorDialog("load [" + chooser.selectedFile.getPath + "] error:", err)
                        case Right(sqls) =>
                            sqlListView.listData = sqls
                            executeAllBtn.enabled = true
                            executeSelectedBtn.enabled = true
                    }
                }
            })
            contents += executeSelectedBtn
            contents += executeAllBtn
            contents += new Button(Action("cancel") {
                if (task != null)
                    task.cancel(true)
                else
                    close
            })
            contents += progressBar
        }
    }

    title = label
    centerOnScreen()
    modal = true
    open()
}
