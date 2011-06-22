package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormPage;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;


public class LibraryPage extends CabalFormPage {

  public Button isALibrary;

  public LibraryPage( final FormEditor editor, final IProject project ) {
    super( editor, LibraryPage.class.getName(), UITexts.cabalEditor_library, project );
  }

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
    top.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    isALibrary = toolkit.createButton( top, UITexts.cabalEditor_isALibrary, SWT.CHECK );
    GridData isLibGD = new GridData();
    isLibGD.horizontalSpan = 2;
    isLibGD.grabExcessHorizontalSpace = true;
    isLibGD.heightHint = 15;
    isLibGD.widthHint = 200;
    isALibrary.setLayoutData( isLibGD );

    CabalFormEditor formEditor = ( CabalFormEditor )getEditor();
    managedForm.addPart( new DependenciesSection( this, top, formEditor, project ) );
    managedForm.addPart( new SourceDirsSection( this, top, formEditor, project ) );
    managedForm.addPart( new ModulesSection( UITexts.cabalEditor_exposedModules, this, top, formEditor, project ) );
    managedForm.addPart( new CompilerOptionsSection( this, top, formEditor, project ) );

    toolkit.paintBordersFor( form );
    this.finishedLoading();
  }

  @Override
  protected void setPackageDescriptionInternal(
      final PackageDescription packageDescription ) {
    // TODO Auto-generated method stub

  }

}
