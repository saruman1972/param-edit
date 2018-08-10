package scala.swing.treetable

import TreeTable.Path
import scala.collection.mutable.ListBuffer
import javax.swing.{tree => jst}
import scala.collection.mutable.ArrayBuffer
import scala.sys.error

import org.jdesktop.swingx.{treetable => jstb}

object ExternalTreeTableModel {
  def empty: ExternalTreeTableModel = new ExternalTreeTableModel(Seq.empty)(Seq.empty)
  def apply(roots: TreeTableNode*)(columnNames: Seq[_]): ExternalTreeTableModel = 
      new ExternalTreeTableModel(roots)(columnNames)
}

/**
 * Represents treetable data as a sequence of root nodes, and a function that can retrieve child nodes.  
 */
class ExternalTreeTableModel(rootItems: Seq[TreeTableNode])(columnNames: Seq[_]) extends TreeTableModel {
  self =>

Console.out.println("columnNames=" + columnNames)  
  import TreeTableModel._
  
  private var rootsVar = List(rootItems: _*)
  
  def roots: Seq[TreeTableNode] = rootsVar
  
  def getChildrenOf(parentPath: Path[TreeTableNode]): Seq[TreeTableNode] = if (parentPath.isEmpty) roots 
                                                                           else parentPath.last.children
  
  def pathToTreePath(path: Path[TreeTableNode]): jst.TreePath = {
    val array = (hiddenRoot +: path).map(_.asInstanceOf[AnyRef]).toArray(ClassManifest.Object)
    new jst.TreePath(array)
  }
  
  def treePathToPath(tp: jst.TreePath): Path[TreeTableNode] = {
    if (tp == null) null 
    else tp.getPath.map(_.asInstanceOf[TreeTableNode]).tail.toIndexedSeq
  } 
  
  /** 
   * A function to update a value in the model, at a given path.  By default this will throw an exception; to 
   * make a TreeModel updatable, call makeUpdatable() to provide a new TreeModel with the specified update method.
   */
  protected[treetable] val updateFunc: (Path[TreeTableNode], TreeTableNode) => TreeTableNode = {
    (_,_) => error("Update is not supported on this tree")
  }
  
  /** 
   * A function to insert a value in the model at a given path, returning whether the operation succeeded.  
   * By default this will throw an exception; to allow insertion on a TreeModel, 
   * call insertableWith() to provide a new TreeModel with the specified insert method.
   */
  protected[treetable] val insertFunc: (Path[TreeTableNode], TreeTableNode, Int) => Boolean = {
    (_,_,_) => error("Insert is not supported on this tree")
  }

  /** 
   * A function to remove a item in the model at a given path, returning whether the operation succeeded.  
   * By default this will throw an exception; to allow removal from a TreeModel, 
   * call removableWith() to provide a new TreeModel with the specified remove method.
   */
  protected[treetable] val removeFunc: Path[TreeTableNode] => Boolean = {
    _ => error("Removal is not supported on this tree")
  }
  
  
  class ExternalTreeTableModelPeer extends jstb.AbstractTreeTableModel {

    def getChildrenOf(parent: Any) = parent match {
      case `hiddenRoot` => roots
      case a: TreeTableNode => a.children
    }
    
    override def getChild(parent: Any, index: Int): AnyRef = {
      val ch = getChildrenOf(parent)
      if (index >= 0 && index < ch.size) 
        ch(index).asInstanceOf[AnyRef] 
      else 
        error("No child of \"" + parent + "\" found at index " + index)
    }
    override def getChildCount(parent: Any): Int = getChildrenOf(parent).size
    override def getIndexOfChild(parent: Any, child: Any): Int = getChildrenOf(parent) indexOf child
    override def getRoot: AnyRef = {
      hiddenRoot
    }
    override def isLeaf(node: Any): Boolean = getChildrenOf(node).isEmpty
    
    override def getValueAt(node: Any, index: Int): AnyRef = {
      node.asInstanceOf[TreeTableNode](index)
    }
    override def getColumnCount(): Int = {
      columnNames.length
    }
    override def getColumnName(column: Int): String = {
      columnNames(column).toString
    }
    override def isCellEditable(node: AnyRef, column: Int) = true
    override def setValueAt(value: AnyRef, node: AnyRef, column: Int) {
Console.out.print("setValueAt(" + value + ")")
      val n = node.asInstanceOf[TreeTableNode]
      n.columns(column) = value
    }
  }
  
  
  
  /**
   * Underlying treetable model that exposes the tree structure to Java Swing.
   *
   * This implementation of javax.swing.tree.TreeModel takes advantage of its abstract nature, so that it respects 
   * the tree shape of the underlying structure provided by the user.
   */
  lazy val peer = new ExternalTreeTableModelPeer
   
}

