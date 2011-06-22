// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
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
class BuildSection extends CabalFormSection {

  BuildSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.advancedPage_cabalSection, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 2, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    MinimalVersionFormEntryCombo<String> combo = new MinimalVersionFormEntryCombo<String>(
        new CabalVersionChoice() );
    createCustomFormEntry( combo, CabalSyntax.FIELD_CABAL_VERSION,
        toolkit, container, UITexts.advancedPage_cabalMinimalVersion, SWT.NONE );
    createComboFormEntry( CabalSyntax.FIELD_BUILD_TYPE, new BuildTypeChoice(),
        toolkit, container, UITexts.advancedPage_cabalBuildType );

    CompilerFormEntry compiler = new CompilerFormEntry();
    createCustomFormEntry( compiler, CabalSyntax.FIELD_TESTED_WITH,
        toolkit, container, UITexts.advancedPage_cabalTestedWith, true,
        SWT.NONE );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }
}
