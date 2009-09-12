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

/** <p>form section for legal info (license, copyright ...).</p>
  *
  * @author Leif Frenzel
  */
class LegalSection extends CabalFormSection {

  private FormEntry txtCopyright;
  private FormEntry txtLicense;
  private FormEntry txtLicenseFile;

  LegalSection( final IFormPage page,
                final Composite parent,
                final CabalFormEditor editor ) {
    super( page, parent, editor, UITexts.legalSection_title );
  }

  @Override
  void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 2, false ) );
    GridData data = new GridData(GridData.FILL_BOTH);
    getSection().setLayoutData( data );

    String text = UITexts.legalSection_entryCopyright;
    txtCopyright = createFormEntry( toolkit, container, text );
    String text2 = UITexts.legalSection_entryLicense;
    txtLicense = createFormEntry( toolkit, container, text2 );
    String text3 = UITexts.legalSection_entryLicenseFile;
    txtLicenseFile = createFormEntry( toolkit, container, text3 );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

  @Override
  void mapData() {
    entries2accs.put( txtCopyright, CabalSyntax.FIELD_COPYRIGHT );
    entries2accs.put( txtLicense, CabalSyntax.FIELD_LICENSE );
    entries2accs.put( txtLicenseFile, CabalSyntax.FIELD_LICENSE_FILE );


  }
}
