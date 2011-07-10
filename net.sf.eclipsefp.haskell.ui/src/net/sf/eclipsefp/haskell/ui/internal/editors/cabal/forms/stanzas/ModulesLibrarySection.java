/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntryMultiSelect;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class ModulesLibrarySection extends CabalFormSection implements IFormEntryListener {

  FormEntryMultiSelect entry;

  ModulesLibrarySection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.cabalEditor_exposedModules, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    entry = new FormEntryMultiSelect( new ModulesContentProvider() );
    setCustomFormEntry( entry, CabalSyntax.FIELD_EXPOSED_MODULES, toolkit,
        container );
    GridData entryGD = new GridData( GridData.FILL_BOTH );
    entryGD.heightHint = 120;
    entry.getControl().setLayoutData( entryGD );
    entry.addFormEntryListener( this );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

  public void refreshInput( final IProject project,
      final PackageDescription descr, final PackageDescriptionStanza stanza ) {
    if( descr == null || stanza == null ) {
      entry.getTree().setInput( new Object() );
    } else {
      String value = stanza.getProperties().get( CabalSyntax.FIELD_EXPOSED_MODULES );
      value = (value == null) ? "" : value;
      entry.getTree().setInput(
          new ModulesContentProviderRoot( project, descr, stanza ) );
      entry.setValue( value, false );
    }
  }

  public void textValueChanged( final FormEntry entry ) {
    // Previouslu, "other-modules" was also updated
    /* if (this.stanza != null) {
      FormEntryMultiSelect multi = (FormEntryMultiSelect)entry;
      String newValue = multi.getNonSelectedValue();
      stanza.getProperties().put( CabalSyntax.FIELD_OTHER_MODULES.getCabalName(), newValue );
      RealValuePosition vp = stanza.update( CabalSyntax.FIELD_OTHER_MODULES, newValue );
      vp.updateDocument( editor.getModel() );
    }*/
  }

  public void focusGained( final FormEntry entry ) {
    // Do nothing
  }

  public void textDirty( final FormEntry entry ) {
    // Do nothing
  }

  public void selectionChanged( final FormEntry entry ) {
    // Do nothing
  }
}
