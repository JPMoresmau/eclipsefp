/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.List;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
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

/**
 * Dialog for adding a dependency to a stanza.
 * @author Alejandro Serrano
 *
 */
public class DependenciesDialog extends Dialog implements
    ISelectionChangedListener, IDoubleClickListener {

  TreeViewer packageTree;
  Text packageName;
  Browser packageDocs;
  String value = null;
  List<String> alreadySelected;

  private final String projectName;

  protected DependenciesDialog( final IShellProvider provider,
      final List<String> alreadySelected,final String projectName ) {
    super( provider );
    this.alreadySelected = alreadySelected;
    this.projectName=projectName;
  }

  protected DependenciesDialog( final Shell parentShell,
      final List<String> alreadySelected, final String projectName ) {
    super( parentShell );
    this.alreadySelected = alreadySelected;
    this.projectName=projectName;
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
    GridData compositeGD = new GridData( GridData.FILL_BOTH );
    compositeGD.heightHint = 300;
    compositeGD.widthHint = 250;
    composite.setLayoutData( compositeGD );
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
        alreadySelected, projectName ) );
    packageTree.setInput( new Object() );

    // Hook for changes in selection
    packageTree.addPostSelectionChangedListener( this );
    // Hook for double clicking
    packageTree.addDoubleClickListener( this );

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

  @Override
  public void selectionChanged( final SelectionChangedEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    Object o = selection.getFirstElement();
    if( o == null ) {
      packageDocs.setText( "" );
      packageName.setText( "" );
    } else {
      HaskellPackage item = ( HaskellPackage )o;
      packageDocs.setText( HtmlUtil.generateDocument( null, item.getDoc() ) );
      StringBuilder sb=new StringBuilder();
      String sep="";
      for (Object o2:selection.toList()){
        sb.append(sep);
        sep=", ";
        sb.append((( HaskellPackage )o2).getIdentifier().getName());
      }
      packageName.setText( sb.toString() );
    }
  }

  @Override
  public void doubleClick( final DoubleClickEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    Object o = selection.getFirstElement();
    if( o == null ) {
      return;
    } else {
      okPressed();
    }
  }
}
