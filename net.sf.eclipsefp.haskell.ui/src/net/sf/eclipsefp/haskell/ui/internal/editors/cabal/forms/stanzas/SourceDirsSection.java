/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.CabalFormSection;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.IFormEntryListener;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * Page section for selecting the 'hs-source-dirs' element of a stanza.
 * @author Alejandro Serrano
 *
 */
public class SourceDirsSection extends CabalFormSection implements IFormEntryListener {

  IFormEntryListener listener = null;

  SourceDirsSection( final IFormPage page, final Composite parent,
      final CabalFormEditor editor, final IProject project ) {
    super( page, parent, editor, UITexts.cabalEditor_sourceDirectories, project );
  }

  @Override
  protected void createClient( final FormToolkit toolkit ) {
    Composite container = toolkit.createComposite( getSection() );
    container.setLayout( new GridLayout( 1, false ) );
    GridData data = new GridData( GridData.FILL_BOTH );
    getSection().setLayoutData( data );

    FormEntry entry = createDirFormEntry( CabalSyntax.FIELD_HS_SOURCE_DIRS, toolkit, container );
    GridData entryGD = new GridData( GridData.FILL_BOTH );
    entryGD.heightHint = 120;
    entry.getControl().setLayoutData( entryGD );
    entry.addFormEntryListener( this );

    toolkit.paintBordersFor( container );
    getSection().setClient( container );
  }

  public void setListener(final IFormEntryListener listener) {
    this.listener = listener;
  }

  public void textValueChanged( final FormEntry entry ) {
    if (listener != null) {
      listener.textValueChanged( entry );
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
