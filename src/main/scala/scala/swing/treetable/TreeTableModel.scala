package scala.swing
package treetable

import scala.Array.fallbackCanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassManifest

import TreeTable.Path
import javax.swing.{tree => jst}
import scala.sys.error

import org.jdesktop.swingx.{treetable => jsxt}

object TreeTableNode {
  def apply(columns: AnyRef*) = {
    new TreeTableNode(ArrayBuffer.empty[AnyRef] ++ columns)
  }
}

class TreeTableNode(val columns: ArrayBuffer[AnyRef]) {
  val children = ArrayBuffer.empty[TreeTableNode]

  def addChild(child: TreeTableNode) { children += child }
  def apply(column: Int): AnyRef = {
    if (column >= columns.length)
      None
    else
      columns(column)
  }
}

object TreeTableModel {
  
  /**
   * This value is the root node of every TreeTableModel's underlying javax.swingx.treetable.TreeTableModel.  As we wish to support multiple root 
   * nodes in a typesafe manner, we need to maintain a permanently hidden dummy root to hang the user's "root" nodes off.
   */
  private[treetable] case object hiddenRoot
  
  def empty: TreeTableModel = new ExternalTreeTableModel(Seq.empty)(Seq.empty)
  def apply(roots: TreeTableNode*)(columnNames: Seq[_]): TreeTableModel = new ExternalTreeTableModel(roots)(columnNames)
}


trait TreeTableModel {
  
  def roots: Seq[TreeTableNode]
  val peer: jsxt.TreeTableModel 
  def getChildrenOf(parentPath: Path[TreeTableNode]): Seq[TreeTableNode]
  def getChildPathsOf(parentPath: Path[TreeTableNode]): Seq[Path[TreeTableNode]] = getChildrenOf(parentPath).map(parentPath :+ _)
  def foreach[U](f: TreeTableNode=>U): Unit = depthFirstIterator foreach f
  
  def pathToTreePath(path: Path[TreeTableNode]): jst.TreePath
  def treePathToPath(tp: jst.TreePath): Path[TreeTableNode]
 
  /**
   * Replace the item at the given path in the tree with a new value. 
   * Events are fired as appropriate.
   */
  
  protected def siblingsUnder(parentPath: Path[TreeTableNode]) = if (parentPath.isEmpty) roots 
                                                                 else getChildrenOf(parentPath)
  

  /**
   * Iterates sequentially through each item in the tree, either in breadth-first or depth-first ordering, 
   * as decided by the abstract pushChildren() method.
   */
  private trait TreeIterator extends Iterator[TreeTableNode] {
    protected var openNodes: Iterator[Path[TreeTableNode]] = roots.map(Path(_)).iterator

    def pushChildren(path: Path[TreeTableNode]): Unit
    def hasNext = openNodes.nonEmpty
    def next() = if (openNodes.hasNext) {
      val path = openNodes.next
      pushChildren(path)
      path.last
    }
    else error("No more items")
  }
  
  def breadthFirstIterator: Iterator[TreeTableNode] = new TreeIterator {
    override def pushChildren(path: Path[TreeTableNode]) {openNodes ++= getChildPathsOf(path).toIterator}
  }
  
  def depthFirstIterator: Iterator[TreeTableNode] = new TreeIterator {
    override def pushChildren(path: Path[TreeTableNode]) {
      val open = openNodes
      openNodes = getChildPathsOf(path).toIterator ++ open // ++'s argument is by-name, and should not directly pass in a var
    }
  }
  
  def size = depthFirstIterator.size
  
  def unpackNode(node: Any): TreeTableNode = node.asInstanceOf[TreeTableNode]
}

