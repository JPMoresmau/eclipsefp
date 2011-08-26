/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * Section for selecting the files to include as data.
 * @author Alejandro Serrano
 *
 */
public class DataFilesSection extends CabalFormSection {

  DataFilesSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.advancedPage_dataFiles, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    FormEntry entry = createFileFormEntry( CabalSyntax.FIELD_DATA_FILES,
        toolkit, container );
    entry.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

}
