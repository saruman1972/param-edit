package com.allinfinance.tools.param.util

import scala.swing._
import Swing._
import scala.util.parsing.combinator._

import javax.swing.UIManager
import javax.swing.plaf.FontUIResource

//import jline.TerminalFactory
import jline.ConsoleReader

object Gui extends SimpleSwingApplication {
// new org.apache.poi.xssf.usermodel.XSSFWorkbook


//UIManager.getDefaults.keys.foreach {key => 
//val value = UIManager.get(key)
//if (value.isInstanceOf[FontUIResource])
//Console.out.println("key=" + key + ", value=" + value)
//                                }
//UIManager.put("swing.boldMetal", false)
val font = UIManager.get("TextField.font")
UIManager.put("RadioButton.font", font)
UIManager.put("Menu.font", font)
UIManager.put("CheckBox.font", font)
UIManager.put("MenuItem.font", font)
UIManager.put("PopupMenu.font", font)
UIManager.put("ComboBox.font", font)
UIManager.put("Label.font", font)


        import ObjTreeNode._
        VerConfigs.load
        val model = new MyExternalTreeModel(VerConfigs.nodes, ((n: Node) => n.children)).makeUpdatableWith {
            (path, node) => 
                path.last.asInstanceOf[ValueTrait].update(node.asInstanceOf[WrapNode].value)
                path.last match {
                    case fn: FieldNode => fn.dirty = true
                    case mn: MapEntryNode => mn.dirty = true
                    case ln: ListEntryNode => ln.dirty = true
                    case _ =>
                }
                path.last
        } makeInsertableWith {
            (parentPath, node, index) =>
                parentPath.last.add(index, node)
                true
        } makeRemovableWith {
            (pathToRemove) =>
                val node = pathToRemove.last
                node.parent.remove(node)
                true
        }


    def top = new MainFrame {
        title = "param edit tool"
        val tree1 = new ObjTreeView(model)
        val tree2 = new ObjTreeView(model)
        contents = new BoxPanel(Orientation.Vertical) {
            contents += new BoxPanel(Orientation.Horizontal) {
                contents += new SplitPane(Orientation.Vertical,  tree1, tree2) {
                    continuousLayout = true
                }
            }
            contents += new BoxPanel(Orientation.Horizontal) {
                // contents += new BoxPanel(Orientation.Horizontal) {
                //     border = TitledBorder(EtchedBorder, "layout")
                //     val a = new RadioButton("Single Tree")
                //     val b = new RadioButton("Double Tree")
                //     val c = new RadioButton("Tree and Table")
                //     val mutex = new ButtonGroup(a, b, c)
                //     contents ++= mutex.buttons
                // }
               contents += new BoxPanel(Orientation.Horizontal) {
                   contents += new Button(Action("save") {
                       for (n <- model.roots) {
                           n.asInstanceOf[VerNode].flushChanges
                           tree1.peer.updateUI
                           tree2.peer.updateUI
                       }
                   })
                   contents += new Button(Action("params load") {
                       import com.allinfinance.tools.param.util.ProductLoad
                       val os = new ParamLoadDialog("load params", ProductLoad.genLoadStmts)
                   })
                   contents += new Button(Action("gl load") {
                       import com.allinfinance.tools.param.util.GlLoad
                       new ParamLoadDialog("load gl", GlLoad.genLoadStmts)
                   })
               }
            }
        }
    }
}

object Repl {
    def main(args: Array[String]): Unit = {
        import Parser.CmdParser
        import Stmt.ExitStmt
        import java.io.{StringReader, BufferedReader, InputStreamReader}
try {
        val console = new ConsoleReader

//        val br = new BufferedReader(new InputStreamReader(System.in))
        var ok = true
        var continue = false
        val parser = new CmdParser
        var line = ""
        while (ok) {
            val prompt = if (continue) "==> " else "PE> "
//          print(prompt)
//            val ln = readLine
            val ln = console.readLine(prompt)
            if (ln == null) ok = false else {
                if (ln.endsWith("\\")) {
                    line += ln.substring(0, ln.size-1)
                    continue = true
                }
                else {
                    line += ln
                    val result = parser.parseAll(parser.cmd, line)
                    result match {
                        case parser.Success(p, _) => p match {
                            case ExitStmt() => ok = false
                            case _ =>
                                p.resolveType
                            p.eval
                        }
                        case f: parser.NoSuccess => println(f)
                    }
                    line = ""
                    continue = false
                }
            }
        }
}
catch {case e: Throwable => e.printStackTrace}
// finally {
//     try {
//         TerminalFactory.get.restore
//     } catch {case e => e.printStackTrace}
// }
    }
}

object ParamUtil {
    def main(args: Array[String]): Unit = {
        if (args.size > 0 && args(0) == "-c")
            Repl.main(args)
        else
            Gui.main(args)
    }
}
