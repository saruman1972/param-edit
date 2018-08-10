package scala.swing
package treetable

import javax.swing.event.TreeModelListener
import javax.swing.JTree
import javax.swing.event.TreeSelectionListener
import scala.collection.SeqView
import javax.swing.{Icon, JComponent}
import scala.swing.event._
import javax.swing.event.CellEditorListener
import scala.collection._
import scala.collection.mutable.{ListBuffer, Buffer, ArrayBuffer}
import scala.reflect.ClassManifest
import java.util.EventObject
import java.io._
import Swing._
import javax.{swing => js}
import js.{tree => jst}
import js.{event => jse}

import org.jdesktop.{swingx => jsx}
import org.jdesktop.swingx.{treetable => jstb}


object TreeTable {

  val Path = IndexedSeq
  type Path[+A] = IndexedSeq[A]
  
  /**
  *  The style of lines drawn between tree nodes.
  */
  object LineStyle extends Enumeration {
    val Angled = Value("Angled")
    val None = Value("None")
    
   
    // "Horizontal" is omitted; it does not display as expected, because of the hidden root; it only shows lines 
    // for the top level.
    // val Horizontal = Value("Horizontal")
  }
  
  object SelectionMode extends Enumeration {
    val Contiguous = Value(jst.TreeSelectionModel.CONTIGUOUS_TREE_SELECTION)
    val Discontiguous = Value(jst.TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION)
    val Single = Value(jst.TreeSelectionModel.SINGLE_TREE_SELECTION)
  }

  private[swing] trait JXTreeTableMixin { def treeTableWrapper: TreeTable }
}



/**
 * Wrapper for a JXTreeTable.  The tree model is represented by a 
 * lazy child expansion function that may or may not terminate in leaf nodes.
 * 
 * @see org.jdesktop.swingx.JXTreeTable
 */
class TreeTable(private var treeTableDataModel: TreeTableModel = TreeTableModel.empty) 
    extends Component 
    with Scrollable.Wrapper { thisTreeTable =>

  import TreeTable._  

  override lazy val peer: jsx.JXTreeTable = new jsx.JXTreeTable(model.peer) with JXTreeTableMixin {
    def treeTableWrapper = thisTreeTable

    // We keep the true root node as an invisible and empty value; the user's data will 
    // always sit visible beneath it.  This is so we can support multiple "root" nodes from the 
    // user's perspective, while maintaining type safety.
    setRootVisible(false)
  }
  
  protected def scrollablePeer = peer

  /**
   * Implicitly converts Tree.Path[A] lists to TreePath objects understood by the underlying peer JTree. 
   * In addition to the objects in the list, the JTree's hidden root node must be prepended.
   */
  implicit def pathToTreePath(p: Path[TreeTableNode]): jst.TreePath = model pathToTreePath p
 
  /**
   * Implicitly converts javax.swing.tree.TreePath objects to Tree.Path[A] lists recognised in Scala Swing.  
   * TreePaths will include the underlying JTree's hidden root node, which is omitted for Tree.Paths.
   */
  implicit def treePathToPath(tp: jst.TreePath): Path[TreeTableNode] = model treePathToPath tp

  def isVisible(path: Path[TreeTableNode]) = peer isVisible path
  def expandPath(path: Path[TreeTableNode]) {peer expandPath path}
  def expandRow(row: Int) {peer expandRow row}

  /**
   * Expands every row. Will not terminate if the tree is of infinite depth.
   */
  def expandAll() {
    var i = 0
    while (i < rowCount) {
      expandRow(i)
      i += 1
    }
  } 
  
  def collapsePath(path: Path[TreeTableNode]) {peer collapsePath path}
  def collapseRow(row: Int) {peer collapseRow row}

  def model = treeTableDataModel
  
  def model_=(tm: TreeTableModel) = {
    treeTableDataModel = tm
  }
  
  /**
   * Collapses all visible rows.
   */
  def collapseAll() {rowCount-1 to 0 by -1 foreach collapseRow}
  
  def isExpanded(path: Path[TreeTableNode]) = peer isExpanded path
  def isCollapsed(path: Path[TreeTableNode]) = peer isCollapsed path
  
  def isEditing() = peer.isEditing()
  
  def editable: Boolean = peer.isEditable
  def editable_=(b: Boolean) {peer.setEditable(b)}
  
  def showsRootHandles = peer.getShowsRootHandles
  def showsRootHandles_=(b:Boolean) { peer.setShowsRootHandles(b) }
  
  def lineStyle = TreeTable.LineStyle withName peer.getClientProperty("JTreeTable.lineStyle").toString
  def lineStyle_=(style: TreeTable.LineStyle.Value) { peer.putClientProperty("JTreeTable.lineStyle", style.toString) }

  
  def rowCount = peer.getRowCount
  def rowHeight = peer.getRowHeight
  def largeModel = peer.isLargeModel
  def scrollableTracksViewportHeight = peer.getScrollableTracksViewportHeight
  def expandsSelectedPaths = peer.getExpandsSelectedPaths
  def expandsSelectedPaths_=(b: Boolean) { peer.setExpandsSelectedPaths(b) }
  def dragEnabled = peer.getDragEnabled
  def dragEnabled_=(b: Boolean) { peer.setDragEnabled(b) }

}
