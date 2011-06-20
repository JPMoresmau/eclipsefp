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

/** <p>form section for general info (name, version, ...).</p>
  *
  * @author Leif Frenzel
  */
class GeneralSection extends CabalFormSection {

  private FormEntry txtName;
  private FormEntry txtVersion;
  private FormEntry txtAuthor;
  private FormEntry txtMaintainer;

  GeneralSection( final IFormPage page,
                  final Composite parent,
                  final CabalFormEditor editor ) {
    super( page, parent, editor, UITexts.generalSection_title );
  }

  @Override
  void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 2, false ) );
    GridData data = new GridData(GridData.FILL_BOTH);
    getSection().setLayoutData( data );

    String text = UITexts.generalSection_entryName;
    txtName = createFormEntry( CabalSyntax.FIELD_NAME, toolkit, container, text );
    String text2 = UITexts.generalSection_entryVersion;
    txtVersion = createFormEntry( CabalSyntax.FIELD_VERSION, toolkit, container, text2 );

    String text3 = UITexts.generalSection_entryAuthor;
    txtAuthor = createFormEntry( CabalSyntax.FIELD_AUTHOR, toolkit, container, text3 );
    String text4 = UITexts.generalSection_entryMaintainer;
    txtMaintainer = createFormEntry( CabalSyntax.FIELD_MAINTAINER, toolkit, container, text4 );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }
}
