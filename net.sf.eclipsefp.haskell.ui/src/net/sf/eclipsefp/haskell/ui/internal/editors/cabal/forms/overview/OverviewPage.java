// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormPage;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.IFormPart;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;

/**
 * <p>
 * The overview page on the Cabal form editor lets the user enter elements of
 * the Cabal descriptions in a form-based UI.
 * </p>
 *
 * @author Leif Frenzel
 */
public class OverviewPage extends CabalFormPage {

  public OverviewPage( final FormEditor editor, final IProject project ) {
    super( editor, OverviewPage.class.getName(), UITexts.overviewPage_title, project );
  }


  // interface methods of FormPage
  // //////////////////////////////

  @Override
  protected void createFormContent( final IManagedForm managedForm ) {
    ScrolledForm form = managedForm.getForm();
    FormToolkit toolkit = managedForm.getToolkit();
    toolkit.decorateFormHeading( form.getForm() );
    form.updateToolBar();
    form.setText( UITexts.overviewPage_title );

    form.getBody().setLayout( createGridLayout( 1, 6, 12 ) );
    Composite top = toolkit.createComposite( form.getBody() );
    top.setLayout( createGridLayout( 2, 0, 0 ) );
    top.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    Composite bottom = toolkit.createComposite( form.getBody() );
    bottom.setLayout( createGridLayout( 1, 0, 0 ) );
    bottom.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    Composite extra = toolkit.createComposite( form.getBody() );
    extra.setLayout( createGridLayout( 2, 0, 0 ) );
    extra.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    CabalFormEditor formEditor = ( CabalFormEditor )getEditor();
    managedForm.addPart( new GeneralSection( this, top, formEditor, project ) );
    managedForm.addPart( new LegalSection( this, top, formEditor, project ) );
    managedForm.addPart( new DescriptionSection( this, bottom, formEditor, project ) );
    managedForm.addPart (new BuildSection(this, extra, formEditor, project));
    managedForm.addPart (new DataFilesSection(this, extra, formEditor, project));

    this.finishedLoading();
  }

  @Override
  protected void setPackageDescriptionInternal(
      final PackageDescription packageDescription ) {
    PackageDescriptionStanza stanza = packageDescription.getPackageStanza();
    for( IFormPart p: getManagedForm().getParts() ) {
      if( p instanceof CabalFormSection ) {
        ( ( CabalFormSection )p ).setStanza( stanza );
      }
    }
  }
}
