import scala.swing._
import scala.swing.event._
import scala.swing.treetable._
import scala.collection.mutable.ListBuffer
import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.JTable
import javax.swing.table.AbstractTableModel
import javax.swing.DropMode
import javax.swing.JComboBox
import javax.swing.DefaultCellEditor
import javax.swing.JTree

import org.jdesktop.swingx.treetable.AbstractTreeTableModel;
import org.jdesktop.swingx.JXTreeTable;

import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.StringSelection
import java.awt.datatransfer.Transferable
import javax.swing.TransferHandler

class MyTreeNode(val name: String, val description: String)
{
    val children = ListBuffer.empty[MyTreeNode]
	
	override def toString: String =	"MyTreeNode: " + name + ", " + description
}

class MyTreeTableModel extends AbstractTreeTableModel 
{
	val myroot = new MyTreeNode( "root", "Root of the tree" )
		
		myroot.children += new MyTreeNode( "Empty Child 1", "This is an empty child" )
		val subtree = new MyTreeNode( "Sub Tree", "This is a subtree (it has children)")
		subtree.children += new MyTreeNode( "EmptyChild 1, 1", "This is an empty child of a subtree" )
		subtree.children += new MyTreeNode( "EmptyChild 1, 2", "This is an empty child of a subtree" )
		myroot.children += subtree
		
		myroot.children += new MyTreeNode( "Empty Child 2", "This is an empty child" )

	override def getColumnCount: Int = 3
	override def getColumnName(column: Int): String = column match
	{
		case 0 => "Name"
		case 1 => "Description"
		case 2 => "Number Of Children"
		case _ => "Unknown"
	}
	override def getValueAt(node: Any, column: Int): AnyRef =
	{
//		Console.out.println( "getValueAt: " + node + ", " + column )
		val treenode = node.asInstanceOf[MyTreeNode]
		column match 
		{
		case 0 => treenode.name
		case 1 => treenode.description
		case 2 => treenode.children.length.toString
		case _ => "Unknown"
		}
	}
	override def getChild(node: AnyRef, index: Int ): AnyRef = node.asInstanceOf[MyTreeNode].children(index)
	override def getChildCount(parent: AnyRef): Int = parent.asInstanceOf[MyTreeNode].children.length
	override def getIndexOfChild(parent: AnyRef, child: AnyRef): Int = 0
	override def isLeaf(node: AnyRef) = 
	 {
		 val treenode = node.asInstanceOf[MyTreeNode]
		 if(treenode.children.length > 0 )
		   false
         else
		   true
	 }
	 override def getRoot: AnyRef = myroot
}

class TableTransferHandler extends TransferHandler {
  override def createTransferable(component: JComponent): Transferable = {
    val table = component.asInstanceOf[JTable]
    val row = table.getSelectedRow()
    val col = table.getSelectedColumn()

    val v = table.getModel.getValueAt(row, col).asInstanceOf[String]
Console.out.println("value=" + v)
    new StringSelection(v)
  }
  override def getSourceActions(component: JComponent): Int = TransferHandler.COPY_OR_MOVE
  override def importData(info: TransferHandler.TransferSupport) = {
Console.out.println("importData")
    if (canImport(info)) {
      val component = info.getComponent().asInstanceOf[JTable]
      val dl = info.getDropLocation().asInstanceOf[JTable.DropLocation]
      val row = dl.getRow()
      val col = dl.getColumn()
Console.out.println("drop location: row=" + row + " col=" + col)

      info.getTransferable().getTransferData(DataFlavor.stringFlavor) match {
        case s: String => {
          component.setValueAt(s, row, col)
          true
        }
        case _ => false
      }
    }
    else
      false
  }
  override def exportDone(component: JComponent, data: Transferable, action: Int) {
    if (action == TransferHandler.MOVE) {
Console.out.println("exportDone")
    }
  }
  override def canImport(info: TransferHandler.TransferSupport) = {
//    if (!info.isDrop) {
//      false
//    }
    if (!info.isDataFlavorSupported(DataFlavor.stringFlavor))
      false
    else
      true
  }
}

class TestTreeTable extends ScrollPane {
	val root1 = TreeTableNode( "Empty Child 1", "This is an empty child" )
	val root2 = TreeTableNode( "Sub Tree", "This is a subtree (it has children)")
	root2.addChild(TreeTableNode( "EmptyChild 1, 1", "This is an empty child of a subtree" ))
	root2.addChild(TreeTableNode( "EmptyChild 1, 2", "This is an empty child of a subtree" ))
	val root3 = TreeTableNode( "Empty Child 2", "This is an empty child" )
   
    val tt = new TreeTable (
//      model = TreeTableModel(root)(List("name", "desc1", "desc2", "desc3", "desc4"))
      TreeTableModel(root1, root2, root3)(List("name", "desc1", "desc2", "desc3", "desc4"))
    )
   
    viewportView = tt
}

object Main extends SimpleSwingApplication {
  val treeTableModel = new MyTreeTableModel()
  
  def top = new MainFrame {
    title = "ScalaTreeTable"
    contents = new BoxPanel(Orientation.Vertical) {
      contents += new TestTreeTable
      peer.add(new JButton("test"))
      peer.add(new JXTreeTable( treeTableModel ))
      contents += new Button {
        text = "hello"
      }
      val table = new Table(25, 30) {
        rowHeight = 25
        autoResizeMode = Table.AutoResizeMode.Off
        showGrid = true
        gridColor = new java.awt.Color(150, 150, 150)
        peer.setDragEnabled(true)
//        peer.setDropMode(DropMode.INSERT_ROWS)
        peer.setDropMode(DropMode.USE_SELECTION)
        peer.setTransferHandler(new TableTransferHandler())

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
Console.out.println("column=" + column)
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

      contents += new ScrollPane {
        viewportView = table
      }
    }
  }
}

