// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.forms.editor.FormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;

/** <p>The overview page on the Cabal form editor lets the user enter elements
  * of the Cabal descriptions in a form-based UI.</p>
  *
  * @author Leif Frenzel
  */
public class OverviewPage extends FormPage {

  public OverviewPage( final FormEditor editor ) {
    super( editor, OverviewPage.class.getName(), UITexts.overviewPage_title );
  }


  // interface methods of FormPage
  ////////////////////////////////

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

    CabalFormEditor formEditor = ( CabalFormEditor )getEditor();
    managedForm.addPart( new GeneralSection( this, top, formEditor ) );
    managedForm.addPart( new LegalSection( this, top, formEditor ) );
    managedForm.addPart( new DescriptionSection( this, bottom, formEditor ) );
  }


  // helping functions
  ////////////////////

  private GridLayout createGridLayout( final int cols,
                                       final int sideMargin,
                                       final int topMargin ) {
    GridLayout layout = new GridLayout( cols, true );
    layout.marginHeight = 0;
    layout.marginWidth = 0;

    layout.marginTop = topMargin;
    layout.marginBottom = topMargin;
    layout.marginLeft = sideMargin;
    layout.marginRight = sideMargin;

    layout.horizontalSpacing = 20;
    layout.verticalSpacing = 17;
    return layout;
  }
}
