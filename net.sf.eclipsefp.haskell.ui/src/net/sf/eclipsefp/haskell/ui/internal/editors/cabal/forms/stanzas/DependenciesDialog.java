package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.List;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;


public class DependenciesDialog extends Dialog implements
    ISelectionChangedListener {

  TreeViewer packageTree;
  Text packageName;
  Browser packageDocs;
  String value = null;
  List<String> alreadySelected;

  protected DependenciesDialog( final IShellProvider provider,
      final List<String> alreadySelected ) {
    super( provider );
    this.alreadySelected = alreadySelected;
  }

  protected DependenciesDialog( final Shell parentShell,
      final List<String> alreadySelected ) {
    super( parentShell );
    this.alreadySelected = alreadySelected;
  }

  public String getValue() {
    return this.value;
  }

  @Override
  protected Control createDialogArea( final Composite parent ) {
    // Set window title
    parent.getShell().setText( UITexts.cabalEditor_addDependency );
    // Create the inside
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    // The layout
    GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    composite.setLayout( layout );

    // Add dependencies
    SashForm form = new SashForm( composite, SWT.VERTICAL );
    form.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    packageTree = new TreeViewer( form );
    packageDocs = new Browser( form, SWT.NONE );
    packageDocs.setFont( packageTree.getControl().getFont() );
    form.setWeights( new int[] { 75, 25 } );

    // Set label provider and sorter
    packageTree.setLabelProvider( new DependenciesDialogLabelProvider() );
    packageTree.setComparator( new ViewerComparator() );
    packageTree.setContentProvider( new DependenciesDialogContentProvider(
        alreadySelected ) );
    packageTree.setInput( null );

    // Hook for changes in selection
    packageTree.addPostSelectionChangedListener( this );

    // Add version text
    packageName = new Text( composite, SWT.SINGLE | SWT.BORDER );
    packageName.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    return composite;
  }

  @Override
  protected void okPressed() {
    value = packageName.getText();
    super.okPressed();
  }

  public void selectionChanged( final SelectionChangedEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    Object o = selection.getFirstElement();
    if( o == null ) {
      packageDocs.setText( "" );
    } else {
      HaskellPackage item = ( HaskellPackage )o;
      packageDocs.setText( HtmlUtil.generateDocument( null, item.getDoc() ) );
      packageName.setText( item.getIdentifier().getName() );
    }
  }
}
