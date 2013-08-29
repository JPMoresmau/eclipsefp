/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersion;
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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * Dialog for adding a dependency to a stanza.
 * @author Alejandro Serrano
 *
 */
public class DependenciesDialog extends Dialog implements
    ISelectionChangedListener, IDoubleClickListener {

  private TreeViewer packageTree;
  private Text packageName;
  private Browser packageDocs;
  private final List<String> alreadySelected;

  private final String projectName;

  /**
   * exact packages selected (with exact versions)
   */
  private final List<HaskellPackage> selectedPackages=new ArrayList<HaskellPackage>();
  /**
   * exact items selection (with version ranges)
   */
  private final List<DependencyItem> selectedItems=new ArrayList<DependencyItem>();

  private Button versionNone;
  private Button versionMajor;
  private Button versionCurrent;
  private Button versionMinor;

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


  public List<DependencyItem> getItems(){
    return selectedItems;
  }

  public List<HaskellPackage> getSelectedPackages() {
    return selectedPackages;
  }

  @Override
  protected void configureShell( final Shell newShell ) {
    super.configureShell( newShell );
    // Set window title
    newShell.setText( UITexts.cabalEditor_addDependency );

  }

  @Override
  protected int getShellStyle() {
    return super.getShellStyle() | SWT.RESIZE;
  }

  @Override
  protected Control createDialogArea( final Composite parent ) {
     // Create the inside
    Composite composite = new Composite( parent, SWT.NONE );
    GridData compositeGD = new GridData( GridData.FILL_BOTH );
    compositeGD.heightHint = 400;
    compositeGD.widthHint = 350;
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
    packageName.setEditable( false );

    Group versionGroup=new Group(composite, SWT.BORDER);
    versionGroup.setText( UITexts.cabalEditor_dependencyVersion );
    versionGroup.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    versionGroup.setLayout( new GridLayout( 1, true ) );

    SelectionAdapter sa=new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent arg0 ) {
         packageName.setText( computeValue() );
      }
    };

    versionNone=new Button(versionGroup,SWT.RADIO);
    versionNone.setText( UITexts.cabalEditor_dependencyVersionNone );

    versionNone.addSelectionListener( sa );

    versionMajor=new Button(versionGroup,SWT.RADIO);
    versionMajor.setText( UITexts.cabalEditor_dependencyVersionMajor );
    versionMajor.addSelectionListener( sa );

    versionCurrent=new Button(versionGroup,SWT.RADIO);
    versionCurrent.setText( UITexts.cabalEditor_dependencyVersionCurrent);
    versionCurrent.addSelectionListener( sa );
    versionCurrent.setSelection( true );

    versionMinor=new Button(versionGroup,SWT.RADIO);
    versionMinor.setText( UITexts.cabalEditor_dependencyVersionMinor);
    versionMinor.addSelectionListener( sa );

    return composite;
  }


  @Override
  public void selectionChanged( final SelectionChangedEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    Object o = selection.getFirstElement();
    this.selectedPackages.clear();

    if( o == null ) {
      this.selectedItems.clear();
      packageDocs.setText( "" );
      packageName.setText( "" );
    } else {
      HaskellPackage item = ( HaskellPackage )o;
      packageDocs.setText( HtmlUtil.generateDocument( null, item.getDoc() ) );
       for (Object o2:selection.toList()){
        this.selectedPackages.add(( HaskellPackage )o2);
      }
      packageName.setText( computeValue() );
    }
  }

  /**
   * compute the string value to show AND the selectedItems list
   * calculate version ranges
   * @return the string value to display
   */
  private String computeValue(){
    StringBuilder sb=new StringBuilder();
    String sep="";
    selectedItems.clear();
    for (HaskellPackage hp:selectedPackages){
      sb.append(sep);
      sep=", ";
      DependencyItem di=new DependencyItem( hp.getIdentifier().getName(), "" );

      if (versionMajor.getSelection()){
        di.setVersion(CabalPackageVersion.getMajorRange( hp.getIdentifier().getVersion() ));
      } else if (versionCurrent.getSelection()){
        di.setVersion(CabalPackageVersion.getMajorRangeFromMinor( hp.getIdentifier().getVersion() ));
      } else if (versionMinor.getSelection()){
        di.setVersion(CabalPackageVersion.getMinorRange( hp.getIdentifier().getVersion() ));
      }
      selectedItems.add( di );
      sb.append(di.toString());
    }
    return sb.toString();
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
