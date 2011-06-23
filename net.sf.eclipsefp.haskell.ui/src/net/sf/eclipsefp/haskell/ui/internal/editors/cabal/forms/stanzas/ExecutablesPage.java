package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormPage;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;


public class ExecutablesPage extends CabalFormPage implements IFormEntryListener {

  private List execsList;
  private final boolean ignoreModify = false;
  private CabalFormEditor formEditor;

  public ExecutablesPage( final FormEditor editor, final IProject project ) {
    super( editor, ExecutablesPage.class.getName(), UITexts.cabalEditor_executables, project );
  }

  SourceDirsSection sourceDirsSection;
  ModulesExecutableSection modulesSection;

  @Override
  protected void createFormContent( final IManagedForm managedForm ) {
    ScrolledForm form = managedForm.getForm();
    FormToolkit toolkit = managedForm.getToolkit();
    toolkit.decorateFormHeading( form.getForm() );
    form.updateToolBar();
    form.setText( UITexts.cabalEditor_executables );

    form.getBody().setLayout( createUnequalGridLayout( 2, 6, 12 ) );

    Composite left = toolkit.createComposite( form.getBody() );
    GridLayout leftLayout = new GridLayout();
    leftLayout.marginHeight = 0;
    leftLayout.marginWidth = 0;
    leftLayout.numColumns = 2;
    left.setLayout( leftLayout );
    left.setLayoutData( new GridData(GridData.FILL_VERTICAL) );

    execsList = new List( left, SWT.SINGLE | SWT.BORDER );
    toolkit.adapt( execsList, true, true );
    GridData listGD = new GridData( GridData.FILL_BOTH );
    listGD.grabExcessHorizontalSpace = true;
    listGD.grabExcessVerticalSpace = true;
    listGD.verticalSpan = 2;
    listGD.widthHint = 200;
    execsList.setLayoutData( listGD );

    Composite buttonComposite = toolkit.createComposite( left );
    GridLayout buttonLayout = new GridLayout();
    buttonLayout.marginHeight = 0;
    buttonLayout.marginWidth = 0;
    buttonLayout.numColumns = 1;
    buttonLayout.makeColumnsEqualWidth = true;
    buttonComposite.setLayout( buttonLayout );

    Button addButton = toolkit.createButton( buttonComposite, UITexts.cabalEditor_add,
        SWT.NONE );
    addButton.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    addButton.addSelectionListener( new SelectionAdapter() {

      @Override
      public void widgetSelected( final SelectionEvent e ) {
        // Do nothing
      }
    } );

    Button removeButton = toolkit.createButton( buttonComposite, UITexts.cabalEditor_remove,
        SWT.NONE );
    removeButton.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    removeButton.addSelectionListener( new SelectionAdapter() {

      @Override
      public void widgetSelected( final SelectionEvent e ) {
        // Do nothing
      }
    } );


    Composite right = toolkit.createComposite( form.getBody() );
    right.setLayout( createGridLayout( 2, 0, 0 ) );
    right.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    formEditor = ( CabalFormEditor )getEditor();
    managedForm.addPart( new DependenciesSection( this, right, formEditor, project ) );
    sourceDirsSection = new SourceDirsSection( this, right, formEditor, project );
    managedForm.addPart( sourceDirsSection );
    modulesSection = new ModulesExecutableSection( this, right, formEditor, project );
    managedForm.addPart( modulesSection );
    managedForm.addPart( new CompilerOptionsSection( this, right, formEditor, project ) );

    sourceDirsSection.setListener( this );

    toolkit.paintBordersFor( form );
    this.finishedLoading();
  }

  @Override
  protected void setPackageDescriptionInternal(
      final PackageDescription packageDescription ) {

    /*ignoreModify = true;
    PackageDescriptionStanza libStanza = packageDescription.getLibraryStanza();
    modulesSection.refreshInput( project, packageDescription, libStanza );
    if (libStanza == null) {
      isALibrary.setSelection( false );
      for( IFormPart p: getManagedForm().getParts() ) {
        if( p instanceof CabalFormSection ) {
          ( ( CabalFormSection )p ).setStanza( null );
        }
      }
    } else {
      isALibrary.setSelection( true );
      for( IFormPart p: getManagedForm().getParts() ) {
        if( p instanceof CabalFormSection ) {
          ( ( CabalFormSection )p ).setStanza( libStanza );
        }
      }
    }
    ignoreModify = false; */
  }

  public void textValueChanged( final FormEntry entry ) {
    /*if (this.getPackageDescription() != null) {
      PackageDescriptionStanza libStanza = this.getPackageDescription().addStanza( CabalSyntax.SECTION_LIBRARY, "" );
      modulesSection.refreshInput( project, this.getPackageDescription(), libStanza );
    } else {
      modulesSection.refreshInput( project, this.getPackageDescription(), null );
    }*/
  }

  public void focusGained( final FormEntry entry ) {
    // Do nothing
  }

  public void textDirty( final FormEntry entry ) {
    // Do nothing
  }

  public void selectionChanged( final FormEntry entry ) {
    // Do nothing
  }
}
