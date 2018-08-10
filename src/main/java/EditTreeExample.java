// Imports
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.DefaultTreeCellEditor;
import javax.swing.tree.DefaultTreeCellRenderer;

public class EditTreeExample
		extends 	JFrame
 {
	// Instance attributes used in this example
	private	JPanel		topPanel;
	private	JTree		tree;
	private	JScrollPane scrollPane;

	// Constructor of main frame
	public EditTreeExample()
	{
		// Set the frame characteristics
		setTitle( "Editable Tree Application" );
		setSize( 300, 100 );
		setBackground( Color.gray );

		// Create a panel to hold all other components
		topPanel = new JPanel();
		topPanel.setLayout( new BorderLayout() );
		getContentPane().add( topPanel );

		// Create a new tree control
		tree = new JTree();
		tree.setEditable( true );
		tree.setShowsRootHandles( false );

        // Create a combo box of editing options
		JComboBox box = new JComboBox();
		box.setMinimumSize( new Dimension( 100, 40 ) );
		box.addItem( "Swing" );
		box.addItem( "Java" );
		box.addItem( "neat" );
		box.addItem( "funky" );
		box.addItem( "life" );
		box.addItem( "awesome" );
		box.addItem( "cool!" );

		// Add a cell editor to the tree
        DefaultTreeCellRenderer renderer = (DefaultTreeCellRenderer) tree.getCellRenderer();
//        TreeCellEditor comboEditor = new DefaultCellEditor(box);
        class Editor extends DefaultCellEditor {
            public Editor(JComboBox comboBox) {
                super(comboBox);
            }

            public Object getCellEditorValue() {
                throw new IllegalArgumentException("hehe");
            }

            public void addCellEditorListener(CellEditorListener l) {
                throw new IllegalArgumentException("hehe");
            }
        }
        TreeCellEditor comboEditor = new Editor(box);
        TreeCellEditor editor = new DefaultTreeCellEditor(tree, renderer, comboEditor);
        for(CellEditorListener ll : ((AbstractCellEditor)comboEditor).getCellEditorListeners())
            System.out.println("listeners=" + ll);
        tree.setCellEditor( editor );
        
		// Add the listbox to a scrolling pane
		scrollPane = new JScrollPane();
		scrollPane.getViewport().add( tree );
		topPanel.add( scrollPane, BorderLayout.CENTER );
	}

	// Main entry point for this example
	public static void main( String args[] )
	{
		// Create an instance of the test application
		EditTreeExample mainFrame	= new EditTreeExample();
        mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		mainFrame.setVisible( true );
	}
}



