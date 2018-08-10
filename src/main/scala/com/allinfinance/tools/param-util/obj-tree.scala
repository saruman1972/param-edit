package com.allinfinance.tools.param.util

import scala.swing._
import scala.swing.event._
import javax.swing.DropMode
import javax.swing.DefaultCellEditor
import javax.swing.JTree
import javax.swing.ToolTipManager
import javax.swing.JComponent
import javax.swing.JComboBox
import javax.swing.JPanel
import javax.swing.JLabel
import javax.swing.JTextField
import javax.swing.UIManager
import Swing._
import javax.{swing => js}
import js.{tree => jst}
import js.{event => jse}
import javax.swing.SwingWorker
import javax.swing.ProgressMonitor
import java.beans.PropertyChangeListener
import java.beans.PropertyChangeEvent

import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.StringSelection
import java.awt.datatransfer.Transferable
import javax.swing.TransferHandler

import scalaswingcontrib.tree._
import scalaswingcontrib.event._

import scala.collection.mutable.ArrayBuffer

import java.lang.reflect.Field
import java.lang.reflect.ParameterizedType
import java.lang.reflect.Modifier
import collection.JavaConversions._

import ObjDef._
import ObjTreeNode._

        class MyExternalTreeModel[A](rootItems: Seq[A], children: A => Seq[A]) extends ExternalTreeModel[A](rootItems, children) {self =>
            override lazy val peer = new ExternalTreeModelPeer {
                override def isLeaf(node: Any) = {
                    node match {
                        case _: VerNode => false
                        case OrgNode(_) => false
                        case ClassNode(_) => false
                        case ObjNode(_) => false
                        case _ => getChildrenOf(node).isEmpty
                    }
                }
                def copyListeners(otherPeer: ExternalTreeModel[A]#ExternalTreeModelPeer) {
                    otherPeer.treeModelListeners foreach addTreeModelListener
                }
            }
            override def makeUpdatableWith(effectfulUpdate: (Tree.Path[A], A) => A): ExternalTreeModel[A] = new MyExternalTreeModel(roots, children) {
                override val updateFunc = effectfulUpdate
                override val insertFunc = self.insertFunc
                override val removeFunc = self.removeFunc
                this.peer copyListeners self.peer
            }

            override def makeInsertableWith(effectfulInsert: (Tree.Path[A], A, Int) => Boolean): ExternalTreeModel[A] = new MyExternalTreeModel(roots, children) {
                override val updateFunc = self.updateFunc
                override val insertFunc = effectfulInsert
                override val removeFunc = self.removeFunc
                this.peer copyListeners self.peer
            }

            override def makeRemovableWith(effectfulRemove: Tree.Path[A] => Boolean): ExternalTreeModel[A] = new MyExternalTreeModel(roots, children) {
                override val updateFunc = self.updateFunc
                override val insertFunc = self.insertFunc
                override val removeFunc = effectfulRemove
                this.peer copyListeners self.peer
            }
        }

class VerEditDialog(verConfig: VerConfig = new VerConfig) extends Dialog {
    var result: Option[VerConfig] = None
    modal = true
    title = "db edit dialog"
    val verName = new TextField(verConfig.name)
    val jdbcUrl = new TextField(verConfig.jdbcUrl)
    val jdbcUserName = new TextField(verConfig.jdbcUserName)
    val jdbcPassword = new TextField(verConfig.jdbcPassword)
    val libPath = new TextField(verConfig.libPath)
    contents = new BoxPanel(Orientation.Vertical) {
        contents += new BoxPanel(Orientation.Horizontal) {
            contents += new GridPanel(5, 1) {
                for (l <- List("name:", "jdbcUrl:", "jdbcUserName:", "jdbcPassword:", "libPath:")) {
                    contents += new Label(l) {
                        horizontalAlignment = Alignment.Right
//                        preferredSize = new java.awt.Dimension(100, 50)
                    }
                }
            }
            contents += new GridPanel(5, 1) {
                contents += verName
                contents += jdbcUrl
                contents += jdbcUserName
                contents += jdbcPassword
                contents += new BoxPanel(Orientation.Horizontal) {
                    contents += libPath
                    contents += new Button(Action("...") {
                        val chooser = new FileChooser {
                            peer.setCurrentDirectory(new java.io.File(libPath.text))
                        }
                        chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
                        if (chooser.showOpenDialog(this) == FileChooser.Result.Approve) {
                            libPath.text = chooser.selectedFile.getPath
                        }
                    })
                }
            }
        }
        contents += new BoxPanel(Orientation.Horizontal) {
            contents += new Button(Action("ok") {
                close
                result = Some(VerConfig(verName.text, jdbcUrl.text, jdbcUserName.text, jdbcPassword.text, libPath.text))
            })
            contents += new Button(Action("cancel") {
                close
            })
        }
    }
        
    centerOnScreen()
    open()
}

class KeyEditDialog(keyType: ObjType, key: AnyRef=null) extends Dialog {
    var result: Option[AnyRef] = None
    modal = true
    title = "key edit dialog"
    contents = new BoxPanel(Orientation.Vertical) {
        contents += new Label("please enter map key")
        contents += new BoxPanel(Orientation.Horizontal) {
            contents += keyType.editor
            if (key != null)
                keyType.setEditValue(key)
        }
        contents += new BoxPanel(Orientation.Horizontal) {
            contents += new Button(Action("ok") {
                close
                result = Some(keyType.getEditValue)
            })
            contents += new Button(Action("cancel") {
                close
            })
        }
    }

    centerOnScreen()
    open()
}

class ObjTreeView(treeModel: TreeModel[Node]) extends ScrollPane {

    class MyTree[A]() extends Tree[A] {
        def fireTreeStructureChanged(path: Tree.Path[A]): Unit = {
            model.asInstanceOf[MyExternalTreeModel[A]].peer.fireTreeStructureChanged(pathToTreePath(path), true)
        }
        def fireTreeNodesChanged(path: Tree.Path[A]): Unit = {
            model.asInstanceOf[MyExternalTreeModel[A]].peer.fireNodesChanged(pathToTreePath(path), true)
        }
    }

    val otree = new MyTree[Node] {thisTree =>
        model = treeModel
        lineStyle = Tree.LineStyle.Angled

        var popupNode: Node = null
        var popupPath: Tree.Path[Node] = null

        val refreshMenuItem = new MenuItem(Action("refresh") {
            popupPath.last match {
                case _: VerNode =>
                    val verNode = popupNode.asInstanceOf[VerNode]
                    verNode.ensureConnected
                    verNode.children.reduceToSize(0)
                    verNode.disconnect
                    fireTreeStructureChanged(popupPath)
                case _: OrgNode =>
                    for (cn <- popupNode.children.toList) {
                        thisTree.model.remove(popupPath :+ cn)
                    }
                    popupNode.asInstanceOf[OrgNode].loaded = false
                    popupNode.asInstanceOf[OrgNode].ensureLoaded
                    fireTreeStructureChanged(popupPath)
                case _: ObjNode =>
                    val on = popupNode.asInstanceOf[ObjNode]
                    for (n <- popupNode.children.toList) {
                        thisTree.model.remove(popupPath :+ n)
                    }
                    popupNode.asInstanceOf[ObjNode].loaded = false
                    popupNode.asInstanceOf[ObjNode].ensureLoaded
                    fireTreeStructureChanged(popupPath)
                case _ =>
            }
        })
        val saveMenuItem = new MenuItem(Action("save changes") {
            for (n <- model.roots) {
                n.asInstanceOf[VerNode].flushChanges
                fireTreeStructureChanged(List(n).toIndexedSeq)
            }
            // thisTree.peer.updateUI
        })
        val newVerMenuItem = new MenuItem(Action("new version defination") {
                (new VerEditDialog).result map {c =>
                    val vn = VerNode(c)
                    thisTree.model.insertUnder(Vector[Node](), vn, VerConfigs.nodes.size)
                    VerConfigs.nodes += vn
                    VerConfigs.flush
                }
        })
        val editVerMenuItem = new MenuItem(Action("edit version defination") {
                val verNode = popupNode.asInstanceOf[VerNode]
                (new VerEditDialog(verNode.verConfig)).result map {c =>
                    verNode.verConfig = c
                    verNode.disconnect
                    verNode.children.reduceToSize(0)
                    fireTreeStructureChanged(popupPath)
                    VerConfigs.flush
                }
        })
        val deleteVerMenuItem = new MenuItem(Action("delete version defination") {
                val verNode = popupNode.asInstanceOf[VerNode]
                thisTree.model.remove(popupPath)                
                VerConfigs.nodes -= verNode
                VerConfigs.flush
        })
        val newOrgMenuItem: MenuItem = new MenuItem(Action("new org") {
                val verNode = popupNode.asInstanceOf[VerNode]
                verNode.ensureConnected
                Dialog.showInput(this, "Please enter the org", "new org node", Dialog.Message.Plain, Swing.EmptyIcon, Nil, "").map {org =>
                    val on = OrgNode(org, verNode)
                    thisTree.model.insertUnder(popupPath, on, verNode.children.size)
                }
        })
        val changeOrgMenuItem: MenuItem = new MenuItem(Action("change org key") {
                val on = popupNode.asInstanceOf[OrgNode]
                Dialog.showInput(this, "Please enter the org", "new org node", Dialog.Message.Plain, Swing.EmptyIcon, Nil, on.org).filter(_ != on.org) map {org => on.org = org}
        })
        val deleteOrgMenuItem = new MenuItem(Action("delete org") {
                thisTree.model.remove(popupPath)
                popupNode.asInstanceOf[OrgNode].removeAll
                popupNode = null  // in case of memory leak
                popupPath = null
        })
        val exportOrgMenuItem = new MenuItem(Action("export org") {
            val on = popupNode.asInstanceOf[OrgNode]
            val dialog = new ParamExportDialog(on.org, on.verNode)
//            dialog.result map {_()}
            // val sheets = List(ParamExport.SheetDesc("总体机构参数", List(("com.allinfinance.bmp.param.def.Organization",false))))
            // ParamExport.export("zzzz.xlsx", on.org, on.verNode, sheets) {
            //     case (sheetDesc,idx) =>
            //         println("export sheet[" + sheetDesc.name, "]")
            // }

            popupNode = null
            popupPath = null
        })
        val newClassMenuItem: MenuItem = new MenuItem(Action("new classes") {
                val on = popupNode.asInstanceOf[OrgNode]
                Dialog.showInput(this, "Please select a class", "new class node", Dialog.Message.Plain, Swing.EmptyIcon, on.parent.asInstanceOf[VerNode].getClasses, "").map {clz =>
                    val cn = ClassNode(Class.forName(clz, true, on.parent.asInstanceOf[VerNode].classLoader), on)
                    thisTree.model.insertUnder(popupPath, cn, on.children.size)
                }
        })
        val deleteClassMenuItem = new MenuItem(Action("delete classes") {
                val cn = popupNode.asInstanceOf[ClassNode]
                cn.ensureLoaded
                cn.parent.dirty = true
                cn.parent.deleteQueue += cn
                thisTree.model.remove(popupPath)
                popupNode.asInstanceOf[ClassNode].removeAll
                popupNode = null  // in case of memory leak
                popupPath = null
        })
        val newObjectMenuItem: MenuItem = new MenuItem(Action("new object") {
                val cn = popupNode.asInstanceOf[ClassNode]
                Dialog.showInput(this, "Please enter the object key for class[" + cn.clazz.getName + "]", "new obj node", Dialog.Message.Plain, Swing.EmptyIcon, Nil, "").map {key =>
                    val prmObject = new PrmObject(cn.parent.asInstanceOf[OrgNode].org, cn.clazz, key)
                    prmObject.paramObject = ObjType(cn.clazz).newInstance
                    val on = ObjNode(prmObject, cn)
                    on.loaded = true
                    on.newBorn = true
                    on.dirty = true
                    on.createChildNodes
                    thisTree.model.insertUnder(popupPath, on, cn.children.size)
                }
        })
        val changeObjectKeyMenuItem: MenuItem = new MenuItem(Action("change object key") {
                val on = popupNode.asInstanceOf[ObjNode]
                Dialog.showInput(this, "Please enter the object key for class[" + on.parent.asInstanceOf[ClassNode].clazz.getName + "]", "new obj node", Dialog.Message.Plain, Swing.EmptyIcon, Nil, on.prmObject.paramKey).filter(_ != on.prmObject.paramKey) map {key =>
                    on.key = key
                }
        })
        val deleteObjectMenuItem = new MenuItem(Action("delete object") {
                val on = popupNode.asInstanceOf[ObjNode]
                val cn = on.parent.asInstanceOf[ClassNode]
                thisTree.model.remove(popupPath)                
                cn.deleteQueue += on
                cn.dirty = true
                popupNode = null
                popupPath = null
        })
        val newFieldValueMenuItem = new MenuItem(Action("new field value") {
            val fn = popupNode.asInstanceOf[FieldNode]
            fn.update(fn.objType.newInstance)
            fn match {
                case _: CompoundTrait => fn.createChildNodes
                case _ =>
            }
            fn.dirty = true
            fireTreeStructureChanged(popupPath)
        })
        val nullFieldValueMenuItem = new MenuItem(Action("set null") {
            val fn = popupNode.asInstanceOf[FieldNode]
            fn.update(null)
            fn.children.reduceToSize(0)
            fn.dirty = true
            fireTreeStructureChanged(popupPath)
        })
        val newEntryMenuItem = new MenuItem(Action("new entry") {
            popupPath.last.dirty = true
            popupPath.last match {
                case mapTrait: MapTrait =>
                    (new KeyEditDialog(popupNode.asInstanceOf[MapTrait].keyType)).result map {key =>
                        if (popupNode.asInstanceOf[ValueTrait].valueOf.asInstanceOf[java.util.Map[AnyRef, AnyRef]].containsKey(key)) {
                            // duplicate key
                            Dialog.showMessage(null, "duplicate key[" + key + "]")
                        }
                        else {
                            val mn = MapEntryNode(key, mapTrait.valueType, popupNode)
                            popupNode.asInstanceOf[ValueTrait].valueOf.asInstanceOf[java.util.Map[AnyRef, AnyRef]].put(key, null)
                            thisTree.model.insertUnder(popupPath, mn, popupNode.children.size)
                        }
                    }
                case listTrait: ListTrait =>
                    val ln = ListEntryNode(popupNode.children.size, listTrait.valueType, popupNode)
                    thisTree.model.insertUnder(popupPath, ln, popupNode.children.size)
                case _ =>
            }
        })
        val changeEntryKeyMenuItem = new MenuItem(Action("change entry key") {
                val mn = popupNode.asInstanceOf[MapEntryNode]
                (new KeyEditDialog(mn.parent.asInstanceOf[MapTrait].keyType, mn.key)).result map {key =>
                    if (mn.parentMap.containsKey(key)) {
                        // duplicate key
                        Dialog.showMessage(null, "duplicate key[" + key + "]")
                    }
                    else {
                        val mv = mn.valueOf          // save entry value temporarily
                        mn.parentMap.remove(mn.key)  // remove old key entry
                        mn.key = key
                        mn.parentMap.put(key, mv)  // add new key entry
                        mn.dirty = true
                        val expanded = isExpanded(popupPath.init)
                        fireTreeStructureChanged(popupPath.init)
                        expandPath(popupPath.init)
                    }
                }
        })
        val deleteEntryMenuItem = new MenuItem(Action("delete entry") {
            popupPath.last.dirty = true
            popupPath.last match {
                case _: MapEntryNode =>
                    val mn = popupNode.asInstanceOf[MapEntryNode]
                    mn.parentMap.remove(mn.key)
                    thisTree.model.remove(popupPath)
                case _: ListEntryNode =>
                    val ln = popupNode.asInstanceOf[ListEntryNode]
                    ln.parentList.remove(ln.index)
                    thisTree.model.remove(popupPath)
                case _ =>
            }
        })
        val popupMenu = new PopupMenu {
            contents += refreshMenuItem
            contents += saveMenuItem
            contents += new Separator
            contents += newVerMenuItem
            contents += editVerMenuItem
            contents += deleteVerMenuItem
            contents += new Separator
            contents += newOrgMenuItem
            contents += changeOrgMenuItem
            contents += deleteOrgMenuItem
            contents += exportOrgMenuItem
            contents += new Separator
            contents += newClassMenuItem
            contents += deleteClassMenuItem
            contents += new Separator
            contents += newObjectMenuItem
            contents += changeObjectKeyMenuItem
            contents += deleteObjectMenuItem
            contents += new Separator
            contents += newFieldValueMenuItem
            contents += nullFieldValueMenuItem
            contents += newEntryMenuItem
            contents += changeEntryKeyMenuItem
            contents += deleteEntryMenuItem
        }

        def showPopupMenu(x: Int, y: Int) = {
            popupPath = treePathToPath(peer.getPathForLocation(x, y))
            if (popupPath != null) {
                popupNode = popupPath.last
                // disable all menuitems
                refreshMenuItem.enabled = false
                saveMenuItem.enabled = model.roots.find (x => x.asInstanceOf[VerNode].dirty) match {
                   case Some(_) => true
                   case None => false
                }
                newVerMenuItem.enabled = false
                editVerMenuItem.enabled = false
                deleteVerMenuItem.enabled = false
                newOrgMenuItem.enabled = false
                changeOrgMenuItem.enabled = false
                deleteOrgMenuItem.enabled = false
                exportOrgMenuItem.enabled = false
                newClassMenuItem.enabled = false
                deleteClassMenuItem.enabled = false
                newObjectMenuItem.enabled = false
                changeObjectKeyMenuItem.enabled = false
                deleteObjectMenuItem.enabled = false
                newFieldValueMenuItem.enabled = false
                nullFieldValueMenuItem.enabled = false
                newEntryMenuItem.enabled = false
                changeEntryKeyMenuItem.enabled = false
                deleteEntryMenuItem.enabled = false

                popupPath.last match {
                    case _: VerNode =>
                        newVerMenuItem.enabled = true
                        editVerMenuItem.enabled = true
                        deleteVerMenuItem.enabled = true
                        newOrgMenuItem.enabled = true
                        refreshMenuItem.enabled = true
                    case _: OrgNode =>
                        changeOrgMenuItem.enabled = true
                        deleteOrgMenuItem.enabled = true
                        exportOrgMenuItem.enabled = true
                        newClassMenuItem.enabled = true
                        refreshMenuItem.enabled = true
                    case _: ClassNode =>
                        deleteClassMenuItem.enabled = true
                        newObjectMenuItem.enabled = true
                    case _: ObjNode =>
                        changeObjectKeyMenuItem.enabled = true
                        deleteObjectMenuItem.enabled = true
                        refreshMenuItem.enabled = true
                    case _: FieldNode =>
                        val fn = popupPath.last.asInstanceOf[FieldNode]
                        popupPath.last match {
                            case _: CompoundTrait =>
                                if (fn.valueOf == null)
                                    newFieldValueMenuItem.enabled = true
                                else
                                    nullFieldValueMenuItem.enabled = true
                            case _: MapTrait =>
                                if (fn.valueOf == null)
                                    newFieldValueMenuItem.enabled = true
                                else
                                    nullFieldValueMenuItem.enabled = true
                                newEntryMenuItem.enabled = true
                            case _: ListTrait =>
                                if (fn.valueOf == null)
                                    newFieldValueMenuItem.enabled = true
                                else
                                    nullFieldValueMenuItem.enabled = true
                                newEntryMenuItem.enabled = true
                            case _ =>
                        }
                    case _: MapEntryNode =>
                        changeEntryKeyMenuItem.enabled = true
                        deleteEntryMenuItem.enabled = true
                    case _: ListEntryNode =>
                        deleteEntryMenuItem.enabled = true
                    case _ =>
                }
                popupMenu.show(this, x, y)
            }
        }

        listenTo(mouse.clicks)
        reactions += {
            // track press event on linux
            case MousePressed(_, point, mod, clicks, pops) =>
                if (pops) {
                    showPopupMenu(point.x, point.y)
                }
            // track release event on windows
            case MouseReleased(_, point, mod, clicks, pops) =>
                if (pops) {
                    showPopupMenu(point.x, point.y)
                }
        }

        peer.setDragEnabled(true)
        peer.setDropMode(DropMode.ON_OR_INSERT)
        peer.setTransferHandler(new TreeTransferHandler())

        renderer = new Tree.DefaultRenderer[Node] {
            override def componentFor(tree: Tree[_], value: Node, info: companion.CellInfo): Component = {
                peer.defaultRendererComponent(tree.peer, value.asInstanceOf[AnyRef], info.isSelected, info.isExpanded, info.isLeaf, info.row, info.hasFocus)
                backgroundNonSelectionColor = if (value.dirty) java.awt.Color.RED else java.awt.Color.WHITE
                value match {
                    case fn: FieldNode => fn.propertyInfo.map {p => tooltip = p.name}
                    case _ => tooltip = null
                }
                this
            }
        }
        ToolTipManager.sharedInstance().registerComponent(peer)

        editable = true
        editor = new Tree.Editor[Node] {thisEditor =>
            private[this] lazy val lazyPeer: jst.TreeCellEditor = new TreeEditorPeer {
                override def isCellEditable(e: java.util.EventObject) = {
                    if (e == null) true  // for key enter start editing
                    else {
                        val tree = getTreeWrapper(e.getSource.asInstanceOf[JTree])
//                            val sn = tree.selection.selectedNode
                        e match {
                            case me: java.awt.event.MouseEvent =>
                                val path = treePathToPath(tree.peer.getPathForLocation(me.getX, me.getY))
                            tree.model.peer.isLeaf(path.last)
                            case _ => false
                        }
                    }
                }
            }
            override def peer = lazyPeer // We can't use a lazy val directly, as Wrapped wouldn't be able to override with a non-lazy val

            listenTo(keys)
            reactions += {
                case KeyPressed(_, Key.Enter, _, _) =>
                    val sel = selection.paths.leadSelection
                    sel.map{path =>
                        if (path.last.children.size == 0) // leaf
                            startEditingAtPath(path)
                    }
            }

            val editingIcon = UIManager.getIcon("Tree.leafIcon")
            object cellEditor extends Component {
                override lazy val peer: JComponent = new JLabel with SuperMixin {
                    val offset = 4 + editingIcon.getIconWidth
                    setLayout(null)

                    def calculateIconY: Int = {
                        val iconHeight = editingIcon.getIconHeight
                        val textHeight = lastComponent.peer.getFontMetrics(lastComponent.peer.getFont).getHeight
                        val textY = iconHeight/2 - textHeight/2
                        val totalY = math.min(0, textY)
                        val totalHeight = math.max(iconHeight, textY + textHeight) - totalY
                        getHeight/2 - (totalY + (totalHeight/2))
                    }
                    override def paint(g: java.awt.Graphics) {
                        val yLoc = calculateIconY
                        editingIcon.paintIcon(this, g, 0, yLoc)
                        super.paint(g)
                    }

                    override def getPreferredSize: Dimension = if (lastComponent == null) new Dimension(0, 0) else {
                        var sz = lastComponent.preferredSize
                        sz.width += offset + 5 + label.preferredSize.width + 2
                        sz.height = math.max(sz.height, editingIcon.getIconHeight)
                        sz.width = math.max(sz.width, 100)
                        sz
                    }

                    override def doLayout: Unit = {
                        label.peer.setBounds(offset, 0, label.preferredSize.width, getHeight)
                        lastComponent.peer.setBounds(offset+label.preferredSize.width+2, 0, getWidth - offset - label.preferredSize.width - 2, getHeight)
                    }
                }

                opaque = false
                border = null

                val label = new Label
                label.opaque = false
//              contents += label
                peer.add(label.peer)

                var lastComponent: Component = null
                reactions += {
                    case MaskEditDone(_) => thisEditor.peer.stopCellEditing
                    case EditDone(_) => thisEditor.peer.stopCellEditing                   // from textfield
                    case SelectionChanged(_) => thisEditor.peer.stopCellEditing           // from combobox
                    case ButtonClicked(_) => thisEditor.peer.stopCellEditing              // from checkbox
                }

                def setComponent(c: Component) {
                    if (lastComponent != null) {
                        deafTo(lastComponent)
//                        contents -= lastComponent
                        peer.remove(lastComponent.peer) // remove all child component
                    }
                    c match {
                        case maskField: MaskedTextField => listenTo(maskField)
                        case textField: TextField => listenTo(textField)
                        case comboBox: ComboBox[_] => listenTo(comboBox.selection)
                        case checkBox: CheckBox => listenTo(checkBox)
                        case _ => listenTo(c)
                    }
//                    contents += c
                    peer.add(c.peer)
                    lastComponent = c
                }
            }

            var currentNode: Node = null
            override def componentFor(tree: Tree[_], a: Node, info: companion.CellInfo): Component = {
                currentNode = a
                val vt = a.asInstanceOf[ValueTrait]
                val et = a.asInstanceOf[EditLabelTrait]
                cellEditor.setComponent(vt.objType.editor)
                cellEditor.label.text = et.editLabel
                vt.objType.setEditValue(vt.valueOf)
                vt.objType.preconfig(a match {
                    case fn: FieldNode => fn.propertyInfo
                    case _ => None
                })
                cellEditor
            }
            override def value = WrapNode(currentNode.asInstanceOf[ValueTrait].objType.getEditValue)
            override def cellEditable = true

//            override def fireCellEditingCancelled() { Console.out.println("editing cancelled") }
//            override def fireCellEditingStopped() { Console.out.println("editing stopped") }

        }
        showsRootHandles = true

        // for lazy loading
        peer.addTreeWillExpandListener(new jse.TreeWillExpandListener {
            override def treeWillExpand(e: jse.TreeExpansionEvent) {
                val path = treePathToPath(e.getPath)
                path.last match {
                    case db: VerNode =>
                        import java.awt.Cursor
                        peer.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR))
                        try {
                            db.ensureConnected
                        }
                        finally {
                            peer.setCursor(Cursor.getDefaultCursor)
                        }
                    case on: OrgNode => on.ensureLoaded
                    case cn: ClassNode => cn.ensureLoaded
                    case on: ObjNode => on.ensureLoaded
                    case _ =>
                }
            }
            override def treeWillCollapse(e: jse.TreeExpansionEvent) {
            }
        })
        
    }

    viewportView = otree

    class TreeTransferHandler extends TransferHandler {
        val dataFlavor: DataFlavor = {
            new DataFlavor(DataFlavor.javaJVMLocalObjectMimeType + ";class=\"" + classOf[Tree.Path[Node]].getName + "\"")
        }
        val flavors = Array(dataFlavor)
        def asTree(component: AnyRef) = {
            // hacking into reflection, cause there's no way to get the real scala obj
            val treeWrapper = component.getClass.getMethod("treeWrapper")
            if (treeWrapper == null)
                None
            else
                Some(treeWrapper.invoke(component).asInstanceOf[MyTree[Node]])
        }

        class TreeTransferable(val path: Tree.Path[Node]) extends Transferable {
            override def getTransferData(flavor: DataFlavor): AnyRef = {
                path
            }
            override def getTransferDataFlavors: Array[DataFlavor] = flavors
            override def isDataFlavorSupported(flavor: DataFlavor) = flavor == dataFlavor
        }

        override def createTransferable(component: JComponent): Transferable = {
            asTree(component) map {tree => new TreeTransferable(tree.selection.paths.toList(0))} getOrElse null
        }
        override def getSourceActions(component: JComponent): Int = TransferHandler.COPY
        override def importData(info: TransferHandler.TransferSupport) = {
            if (canImport(info)) {
                asTree(info.getComponent) map { tree =>
                    val dl = info.getDropLocation().asInstanceOf[JTree.DropLocation]
                    val srcPath = info.getTransferable().getTransferData(dataFlavor).asInstanceOf[Tree.Path[Node]]
                    val dstPath = tree.treePathToPath(dl.getPath)
//                    val dstPath = tree.selection.selectedNode
                    srcPath.last match {
                        case on: OrgNode =>
                            val (dn, isOverwrite, overwritePath) =
                                if (dstPath.size < srcPath.size) {
                                    dstPath.last.children.find {n => n.asInstanceOf[OrgNode].org == on.org} match {
                                        case Some(n) =>
                                            n.asInstanceOf[OrgNode].removeAll
                                            (n.asInstanceOf[OrgNode], true, dstPath :+ n)
                                        case None =>
                                            val n = OrgNode(on.org, dstPath.last.asInstanceOf[VerNode])
                                            n.loaded = true
                                            tree.model.insertUnder(dstPath, n, dstPath.last.children.size)
                                            (n, false, null)
                                    }
                                }
                                else {
                                    val n = dstPath.last.asInstanceOf[OrgNode]
                                    n.removeAll
                                    (n, true, dstPath)
                                }
                            // overwrite
                            on.ensureLoaded
                            on.children.foreach {cn =>
                                cn.asInstanceOf[ClassNode].ensureLoaded
                                val dstClassNode = ClassNode(Class.forName(cn.asInstanceOf[ClassNode].clazz.getName, true, dn.parent.asInstanceOf[VerNode].classLoader), dn)
                                dstClassNode.loaded = true
                                dn.add(dstClassNode)
                                cn.children.foreach {n =>
                                    n.asInstanceOf[ObjNode].ensureLoaded
                                    dstClassNode.add(ObjNode(n.asInstanceOf[ObjNode], dstClassNode))
                                }
                            }
                            if (isOverwrite) {
                                // update ui
                                val expanded = tree.isExpanded(overwritePath)
                                tree.fireTreeStructureChanged(overwritePath)
                                if (expanded)
                                    tree.expandPath(overwritePath)
                            }
                        case cn: ClassNode =>
                            val (dn, isOverwrite, overwritePath) = 
                                if (dstPath.size < srcPath.size) {
                                    dstPath.last.asInstanceOf[OrgNode].children.find {n => n.asInstanceOf[ClassNode].clazz.getName == cn.clazz.getName} match {
                                        case Some(n) =>
                                            // clear all objects first
                                            n.asInstanceOf[ClassNode].removeAll
                                            (n.asInstanceOf[ClassNode], true, dstPath :+ n)
                                        case None =>
                                            val on = dstPath.last.asInstanceOf[OrgNode]
                                            val n = new ClassNode(Class.forName(cn.clazz.getName, true, on.parent.asInstanceOf[VerNode].classLoader), dstPath.last.asInstanceOf[OrgNode])
                                            n.loaded = true
                                            tree.model.insertUnder(dstPath, n, dstPath.last.children.size)
                                            (n, false, null)
                                    }
                                }
                                else {
                                    val n = dstPath.last.asInstanceOf[ClassNode]
                                    n.removeAll
                                    (n, true, dstPath)
                                }
                            // overwrite
                            cn.ensureLoaded
                            if (cn.children.size > 100) {
                                val task = new MyWorker("copy class[" + cn.clazz + "]") {
                                // val progressMonitor = new ProgressMonitor(otree.peer, "copy class[" + cn.clazz + "]", "", 0, 100)
                                // val task = new SwingWorker[Unit, Unit] {
                                //     addPropertyChangeListener(new PropertyChangeListener {
                                //         override def propertyChange(evt: PropertyChangeEvent) {
                                //             if (evt.getPropertyName == "progress") {
                                //                 val i = evt.getNewValue.asInstanceOf[java.lang.Integer]
                                //                 progressMonitor.setProgress(i)
                                //                 progressMonitor.setNote("Completed %02d%%.\n".format(i))
                                //                 if (progressMonitor.isCanceled()) {
                                //                     cancel(true)  // task.cancel(true)
                                //                 }
                                //             }
                                //         }
                                //     })

                                    override def doInBackground {
                                        try {
                                            setProgress(0)
                                            var i = 1
                                            cn.children.foreach {on =>
                                                if (isCancelled) throw new IllegalArgumentException("cancelled")
                                                on.asInstanceOf[ObjNode].ensureLoaded
                                                dn.add(ObjNode(on.asInstanceOf[ObjNode], dn))
                                                setProgress(math.min(i*100/cn.children.size, 100))
                                                i += 1
                                            }
                                        }
                                        catch {case e: Throwable =>}
                                    }
                                }
                                task.execute
                            }
                            else {
                                cn.children.foreach {on =>
                                    on.asInstanceOf[ObjNode].ensureLoaded
                                    dn.add(ObjNode(on.asInstanceOf[ObjNode], dn))
                                }
                            }
                            if (isOverwrite) {
                                // update ui
                                val expanded = tree.isExpanded(overwritePath)
                                tree.fireTreeStructureChanged(overwritePath)
                                if (expanded)
                                    tree.expandPath(overwritePath)
                            }
                        case on: ObjNode =>
                            on.ensureLoaded
                            if (dstPath.size < srcPath.size) {
                                dstPath.last.add(ObjNode(on, dstPath.last.asInstanceOf[ClassNode]))
                            }
                            else {
                                val dn = dstPath.last.asInstanceOf[ObjNode]
                                dn.cloneFrom(on)
                            }
                            tree.fireTreeStructureChanged(dstPath)
                        case fn: FieldNode =>
                            val dn = dstPath.last.asInstanceOf[FieldNode]
                            dn.cloneFrom(fn)
                            tree.fireTreeStructureChanged(dstPath)
                        case mn: MapEntryNode =>
                            if (dstPath.size < srcPath.size) {
                                val dstMapTrait = dstPath.last.asInstanceOf[MapTrait]
                                dstPath.last.children.find(n => n.asInstanceOf[MapEntryNode].key.toString == mn.key.toString) match {
                                    case Some(n: MapEntryNode) => n.cloneFrom(mn)
                                    case None =>
                                        val dstMap =
                                            if (dstMapTrait.valueOf == null) {
                                                val m = dstMapTrait.objType.newInstance
                                                dstMapTrait.update(m)
                                                m.asInstanceOf[java.util.Map[AnyRef, AnyRef]]
                                            }
                                            else
                                                dstMapTrait.valueOf.asInstanceOf[java.util.Map[AnyRef, AnyRef]]
                                        val dstKey = dstMapTrait.keyType.clone(mn.key, mn.parent.asInstanceOf[MapTrait].keyType)
                                        val n = MapEntryNode(dstKey, dstMapTrait.valueType, dstPath.last)
                                        n.cloneFrom(mn)
                                        dstPath.last.add(n)
                                }
                            }
                            else
                                dstPath.last.asInstanceOf[MapEntryNode].cloneFrom(mn)
                            tree.fireTreeStructureChanged(dstPath)
                        case ln: ListEntryNode =>
                            if (dstPath.size < srcPath.size) {
                                val dstListTrait = dstPath.last.asInstanceOf[ListTrait]
                                if ((srcPath.init == dstPath) && (dl.getChildIndex >= 0)) {
                                    // same list, reorder
                                    dstListTrait.relocate(ln.index, dl.getChildIndex)
                                }
                                else {
                                    val dstList =
                                        if (dstListTrait.valueOf == null) {
                                            val l = dstListTrait.objType.newInstance
                                            dstListTrait.update(l)
                                            l.asInstanceOf[java.util.List[AnyRef]]
                                        }
                                        else
                                            dstListTrait.valueOf.asInstanceOf[java.util.List[AnyRef]]
                                    dstListTrait.valueOf.asInstanceOf[java.util.List[AnyRef]].add(null)
                                    dstList.add(null)
                                    val n = ListEntryNode(dstPath.last.children.size, dstListTrait.valueType, dstPath.last)
                                    n.cloneFrom(ln)
                                    dstPath.last.add(n)
                                }
                            }
                            else
                                dstPath.last.asInstanceOf[ListEntryNode].cloneFrom(ln)
                            tree.fireTreeStructureChanged(dstPath)
                        case _ =>
                    }
                    true
                } getOrElse false
            }
            else
                false 
        }
        override def exportDone(component: JComponent, data: Transferable, action: Int) {
            // if (action == TransferHandler.MOVE) {
            //     Console.out.println("exportDone")
            // }
        }
        override def canImport(info: TransferHandler.TransferSupport) = {
            if (!info.isDrop)
                false
            info.setShowDropLocation(true)
            if (!info.isDataFlavorSupported(dataFlavor))
                false
            asTree(info.getComponent) map { tree =>
                val dl = info.getDropLocation().asInstanceOf[JTree.DropLocation]
                val srcPath = info.getTransferable().getTransferData(dataFlavor).asInstanceOf[Tree.Path[Node]]
                val dstPath = tree.treePathToPath(dl.getPath)
                srcPath.last match {
                    case on: OrgNode =>
                        if ((srcPath.size == dstPath.size) && (srcPath != dstPath))
                            true
                        else if ((srcPath.size -1 == dstPath.size) && (srcPath.init != dstPath))
                            true
                        else
                            false
                    case cn: ClassNode =>
                        if ((srcPath.size == dstPath.size) && (srcPath != dstPath) &&
                            (cn.clazz.getName == dstPath.last.asInstanceOf[ClassNode].clazz.getName))
                            true
                        else if ((srcPath.size-1 == dstPath.size) && (srcPath.init != dstPath))
                            true
                        else
                            false
                    case on: ObjNode =>
                        if ((srcPath.size == dstPath.size) && (srcPath != dstPath))
                            true
                        else if ((srcPath.size-1 == dstPath.size) && (srcPath.init != dstPath) &&
                             (srcPath(srcPath.size-2).asInstanceOf[ClassNode].clazz.getName == dstPath.last.asInstanceOf[ClassNode].clazz.getName))
                            true
                        else
                            false
                    case fn: FieldNode =>
                        if ((srcPath.size == dstPath.size) && (srcPath != dstPath) && 
                            (fn.fd.getName == dstPath.last.asInstanceOf[FieldNode].fd.getName) &&
                            !(fn.parentObj eq dstPath.last.asInstanceOf[FieldNode].parentObj))
                            true
                        else
                            false
                    case mn: MapEntryNode =>
                        if ((srcPath.size == dstPath.size) && (srcPath != dstPath))
                            true
                        else if ((srcPath.size-1 == dstPath.size) && (srcPath.init != dstPath) && (srcPath.init.init != dstPath.init))
                            true
                        else
                            false
                    case ln: ListEntryNode =>
                        if ((srcPath.size == dstPath.size) && (srcPath != dstPath))
                            true
                        else if (srcPath.size-1 == dstPath.size) {
                            if (srcPath.init == dstPath) {
                                // same list
                                if (dl.getChildIndex == -1)
                                    false
                                else        // insert
                                    true
                            }
                            else if (srcPath.init.init != dstPath.init)
                                true
                            else
                                false
                        }
                        else
                            false
                    case _ =>
                        false
                }
            } getOrElse false
        }
    }
}

