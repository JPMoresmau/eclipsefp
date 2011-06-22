package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntryMultiSelect;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class CompilerOptionsSection extends CabalFormSection {

  CompilerOptionsSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.cabalEditor_compilerOptions, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 2, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    createFormEntry( CabalSyntax.FIELD_GHC_OPTIONS, toolkit, container,
        UITexts.cabalEditor_compilerFlags );
    FormEntry entry = new FormEntryMultiSelect(
        new CompilerExtensionContentProvider() );
    createCustomFormEntry( entry, CabalSyntax.FIELD_EXTENSIONS, toolkit,
        container, UITexts.cabalEditor_compilerExtensions, true, SWT.NONE );
    GridData entryGD = (GridData)entry.getControl().getLayoutData();
    entryGD.heightHint = 100;
    entry.getControl().setLayoutData( entryGD );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

}
