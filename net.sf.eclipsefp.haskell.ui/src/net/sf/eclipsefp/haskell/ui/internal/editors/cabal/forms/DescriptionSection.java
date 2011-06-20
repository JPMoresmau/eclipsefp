// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;

/** <p>form section for description info (synopsis, description ...).</p>
  *
  * @author Leif Frenzel
  */
class DescriptionSection extends CabalFormSection {

  private FormEntry txtSynopsis;
  private FormEntry txtDescription;
  private FormEntry txtHomepage;
  private FormEntry txtCategory;

  DescriptionSection( final IFormPage page,
                      final Composite parent,
                      final CabalFormEditor editor ) {
    super( page, parent, editor, UITexts.descriptionSection_title );
  }

  @Override
  void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 2, false ) );
    GridData data = new GridData(GridData.FILL_BOTH);
    getSection().setLayoutData( data );

    String text = UITexts.descriptionSection_entrySynopsis;
    txtSynopsis = createFormEntry( CabalSyntax.FIELD_SYNOPSIS, toolkit, container, text );
    String text2 = UITexts.descriptionSection_entryDescription;
    txtDescription = createMultiLineFormEntry( CabalSyntax.FIELD_DESCRIPTION, toolkit, container, text2 );
    String text3 = UITexts.descriptionSection_entryHomepage;
    txtHomepage = createFormEntry( CabalSyntax.FIELD_HOMEPAGE, toolkit, container, text3 );
    String text4 = UITexts.descriptionSection_entryCategory;
    txtCategory = createFormEntry( CabalSyntax.FIELD_CATEGORY, toolkit, container, text4 );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }
}
