package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntryMultiSelect;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;


public class ModulesExecutableSection extends CabalFormSection implements IFormEntryListener {

  FormEntryMultiSelect entry;

  ModulesExecutableSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.cabalEditor_mainModule, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    entry = new MainIsFormEntry( new ModulesContentProvider() );
    entry.init( getProject(), container, toolkit, SWT.NONE );
    entry.setProperty( CabalSyntax.FIELD_MAIN_IS );
    entry.addFormEntryListener( this );
    // entries.add( entry );
    GridData entryGD = new GridData( GridData.FILL_BOTH );
    entryGD.heightHint = 120;
    entry.getControl().setLayoutData( entryGD );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

  @Override
  public void setAllEditable( final boolean editable ) {
    super.setAllEditable( editable );
    entry.setEditable( editable );
  }

  public void refreshInput( final IProject project,
      final PackageDescription descr, final PackageDescriptionStanza stanza ) {
    if( descr == null || stanza == null ) {
      entry.getTree().setInput( new Object() );
    } else {
      String value = stanza.getProperties().get( CabalSyntax.FIELD_MAIN_IS );
      value = (value == null) ? "" : value;
      int prevCount = entry.getTree().getTree().getItemCount();
      entry.getTree().setInput(
          new ModulesContentProviderRoot( project, descr, stanza ) );
      int postCount = entry.getTree().getTree().getItemCount();
      entry.setValue( value, true );
      // We have to rewrite the "other-modules" section
      if (prevCount != postCount) {
        textValueChanged( entry );
      }
    }
  }

  public void textValueChanged( final FormEntry entry ) {
    if (this.stanza != null) {
      FormEntryMultiSelect multi = (FormEntryMultiSelect)entry;

      String newValueMain = multi.getValue();
      String oldValueMain = stanza.getProperties().get( CabalSyntax.FIELD_MAIN_IS.getCabalName() );
      oldValueMain = oldValueMain == null ? "" : oldValueMain;
      if (!oldValueMain.equals( newValueMain )) {
        stanza.getProperties().put( CabalSyntax.FIELD_MAIN_IS.getCabalName(), newValueMain );
        RealValuePosition vp = stanza.update( CabalSyntax.FIELD_MAIN_IS, newValueMain );
        vp.updateDocument( editor.getModel() );
      }

      String newValue = multi.getNonSelectedValue();
      String oldValue = stanza.getProperties().get( CabalSyntax.FIELD_OTHER_MODULES.getCabalName() );
      oldValue = oldValue == null ? "" : oldValue;
      if (!oldValue.equals( newValue )) {
        stanza.getProperties().put( CabalSyntax.FIELD_OTHER_MODULES.getCabalName(), newValue );
        RealValuePosition vp = stanza.update( CabalSyntax.FIELD_OTHER_MODULES, newValue );
        vp.updateDocument( editor.getModel() );
      }
    }
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
