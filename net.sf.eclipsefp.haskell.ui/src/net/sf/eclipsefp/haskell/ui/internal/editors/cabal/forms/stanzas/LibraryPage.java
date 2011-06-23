package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormPage;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.IFormPart;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;


public class LibraryPage extends CabalFormPage implements SelectionListener, IFormEntryListener {

  private Button isALibrary;
  private boolean ignoreModify = false;
  private CabalFormEditor formEditor;

  public LibraryPage( final FormEditor editor, final IProject project ) {
    super( editor, LibraryPage.class.getName(), UITexts.cabalEditor_library, project );
  }

  SourceDirsSection sourceDirsSection;
  ModulesLibrarySection modulesSection;

  @Override
  protected void createFormContent( final IManagedForm managedForm ) {
    ScrolledForm form = managedForm.getForm();
    FormToolkit toolkit = managedForm.getToolkit();
    toolkit.decorateFormHeading( form.getForm() );
    form.updateToolBar();
    form.setText( UITexts.cabalEditor_library );

    form.getBody().setLayout( createGridLayout( 1, 6, 12 ) );
    Composite top = toolkit.createComposite( form.getBody() );
    top.setLayout( createGridLayout( 2, 0, 0 ) );
    top.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    isALibrary = toolkit.createButton( top, UITexts.cabalEditor_isALibrary, SWT.CHECK );
    GridData isLibGD = new GridData();
    isLibGD.horizontalSpan = 2;
    isLibGD.grabExcessHorizontalSpace = true;
    isLibGD.heightHint = 15;
    isLibGD.widthHint = 200;
    isALibrary.setLayoutData( isLibGD );
    isALibrary.addSelectionListener( this );

    formEditor = ( CabalFormEditor )getEditor();
    managedForm.addPart( new DependenciesSection( this, top, formEditor, project ) );
    sourceDirsSection = new SourceDirsSection( this, top, formEditor, project );
    managedForm.addPart( sourceDirsSection );
    modulesSection = new ModulesLibrarySection( this, top, formEditor, project );
    managedForm.addPart( modulesSection );
    managedForm.addPart( new CompilerOptionsSection( this, top, formEditor, project ) );

    sourceDirsSection.setListener( this );

    toolkit.paintBordersFor( form );
    this.finishedLoading();
  }

  @Override
  protected void setPackageDescriptionInternal(
      final PackageDescription packageDescription ) {

    ignoreModify = true;
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
    ignoreModify = false;
  }

  public void widgetSelected( final SelectionEvent e ) {
    if (!ignoreModify) {
      PackageDescription lastDescription = this.formEditor.getPackageDescription();
      if (isALibrary.getSelection()) {
        // We need to add a stanza
        PackageDescriptionStanza libStanza = lastDescription.addStanza( CabalSyntax.SECTION_LIBRARY, "" );
        libStanza.setIndent( 2 );
        libStanza.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, "src" );
        libStanza.update( CabalSyntax.FIELD_GHC_OPTIONS, "-Wall" );
        formEditor.getModel().set( lastDescription.dump() );
      } else {
        // We need to remove the stanza
        PackageDescriptionStanza libStanza = lastDescription.getLibraryStanza();
        lastDescription.removeStanza( libStanza );
        formEditor.getModel().set( lastDescription.dump() );
      }
    }
  }

  public void textValueChanged( final FormEntry entry ) {
    if (this.getPackageDescription() != null) {
      PackageDescriptionStanza libStanza = this.getPackageDescription().addStanza( CabalSyntax.SECTION_LIBRARY, "" );
      modulesSection.refreshInput( project, this.getPackageDescription(), libStanza );
    } else {
      modulesSection.refreshInput( project, this.getPackageDescription(), null );
    }
  }

  public void widgetDefaultSelected( final SelectionEvent e ) {
    // Do nothing
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
