/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
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


public class ModulesExecutableSection extends CabalFormSection implements IOtherValueEntryListener {

  FormEntryMainIs entry;

  ModulesExecutableSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.cabalEditor_modules, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    entry = new FormEntryMainIs( UITexts.cabalEditor_main_modules );
    setCustomFormEntry( entry, CabalSyntax.FIELD_MAIN_IS, toolkit,
        container );
    GridData entryGD = new GridData( GridData.FILL_BOTH );
    entryGD.heightHint = 120;
    entry.getControl().setLayoutData( entryGD );
    entry.addOtherValueListener( this );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

  @Override
  public void setAllEditable( final boolean editable ) {
    super.setAllEditable( editable );
    entry.setEditable( editable );
  }

  public void refreshInput( final IProject project,
      final PackageDescription descr, final PackageDescriptionStanza stanza,
      final boolean blockNotification ) {
    if( descr == null || stanza == null ) {
      entry.setSourceFolders( null, blockNotification );
    } else {
      entry
          .setSourceFolders(
              new FormEntryModulesRoot( project, descr, stanza ),
              blockNotification );
    }
  }

  public void otherTextValueChanged( final FormEntry entry ) {
    if( this.stanza != null ) {
      FormEntryMainIs multi = ( FormEntryMainIs )entry;
      String newValue = multi.getOtherModulesValue();
      stanza.getProperties().put(
          CabalSyntax.FIELD_OTHER_MODULES.getCabalName(), newValue );
      RealValuePosition vp = stanza.update( CabalSyntax.FIELD_OTHER_MODULES,
          newValue );
      vp.updateDocument( editor.getModel() );
    }
  }

  @Override
  public void setStanza( final PackageDescriptionStanza stanza ) {
    super.setStanza( stanza );
    if (stanza != null) {
      entry.setOtherModulesValue( stanza.getProperties().get( CabalSyntax.FIELD_OTHER_MODULES.getCabalName() ), true );
    }
  }
}
