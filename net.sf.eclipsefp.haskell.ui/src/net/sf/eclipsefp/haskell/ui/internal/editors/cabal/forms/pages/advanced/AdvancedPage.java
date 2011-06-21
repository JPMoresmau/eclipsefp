package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.pages.advanced;

import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormPage;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.layout.GridData;
import org.eclipse.ui.forms.IFormPart;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;


public class AdvancedPage extends CabalFormPage {

  public AdvancedPage( final FormEditor editor ) {
    super( editor, AdvancedPage.class.getName(), UITexts.advancedPage_title );
  }

  @Override
  protected void createFormContent( final IManagedForm managedForm ) {
    ScrolledForm form = managedForm.getForm();
    FormToolkit toolkit = managedForm.getToolkit();
    toolkit.decorateFormHeading( form.getForm() );
    form.updateToolBar();
    form.setText( UITexts.advancedPage_title );
    form.getBody().setLayout( createGridLayout( 2, 6, 12 ) );

    CabalFormEditor formEditor = ( CabalFormEditor )getEditor();

    CabalSection cabalSection = new CabalSection( this, form.getBody(), formEditor );
    managedForm.addPart( cabalSection );
    cabalSection.getSection().setLayoutData( new GridData (GridData.FILL_VERTICAL) );

    DataFilesSection dataSection = new DataFilesSection( this, form.getBody(), formEditor );
    managedForm.addPart( dataSection );
    dataSection.getSection().setLayoutData( new GridData (GridData.FILL_VERTICAL) );

    this.finishedLoading();
  }

  @Override
  public void setPackageDescriptionInternal( final PackageDescription packageDescription ) {
    PackageDescriptionStanza stanza = packageDescription.getPackageStanza();
    for (IFormPart p:getManagedForm().getParts()){
       if (p instanceof CabalFormSection){
         ((CabalFormSection)p).setStanza( stanza );
       }
     }
  }
}
