package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class ModulesSection extends CabalFormSection {

  ModulesSection( final String title, final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, title, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    /*create
    DependenciesFormEntry entry = new
    setCustomFormEntry( entry, CabalSyntax.FIELD_BUILD_DEPENDS, toolkit,
        container );
    GridData entryGD = new GridData( GridData.FILL_BOTH );
    entryGD.heightHint = 120;
    entry.getControl().setLayoutData( entryGD );*/

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

}
