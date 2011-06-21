package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.pages.advanced;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class DataFilesSection extends CabalFormSection {

  DataFilesSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor ) {
    super( page, parent, editor, UITexts.advancedPage_dataFiles );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    Label infoLabel = toolkit.createLabel( container, UITexts.advancedPage_selectDataFiles );
    infoLabel.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
  }

}
