// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * <p>
 * form section for legal info (license, copyright ...).
 * </p>
 *
 * @author Leif Frenzel
 */
class LegalSection extends CabalFormSection {

  LegalSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.legalSection_title, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 2, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    String text = UITexts.legalSection_entryCopyright;
    createFormEntry( CabalSyntax.FIELD_COPYRIGHT, toolkit, container, text );
    String text2 = UITexts.legalSection_entryLicense;
    createComboFormEntry( CabalSyntax.FIELD_LICENSE, new LicenseChoice(),
        toolkit, container, text2 );
    String text3 = UITexts.legalSection_entryLicenseFile;
    createFormEntry( CabalSyntax.FIELD_LICENSE_FILE, toolkit, container, text3 );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }
}
