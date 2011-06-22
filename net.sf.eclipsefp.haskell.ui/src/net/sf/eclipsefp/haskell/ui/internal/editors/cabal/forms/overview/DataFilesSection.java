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
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;


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

    Label infoLabel = toolkit.createLabel( container,
        UITexts.advancedPage_selectDataFiles );
    GridData labelGD = new GridData( GridData.VERTICAL_ALIGN_BEGINNING );
    labelGD.grabExcessHorizontalSpace = true;
    infoLabel.setLayoutData( labelGD );

    FormEntry entry = createFileFormEntry( CabalSyntax.FIELD_DATA_FILES,
        toolkit, container );
    entry.getControl().setLayoutData( new GridData( GridData.FILL_BOTH ) );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

}
