// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.internal.project.IManipulateCabalFile;
import net.sf.eclipsefp.haskell.core.internal.project.InvalidCabalFileException;
import net.sf.eclipsefp.haskell.core.internal.project.IManipulateCabalFile.Accessor;
import net.sf.eclipsefp.haskell.core.internal.project.IManipulateCabalFile.Mutator;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.SectionPart;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

/** <p>form section super class - encapsulates some common functionality.</p>
  *
  * @author Leif Frenzel
  */
abstract class CabalFormSection extends SectionPart {

  final CabalFormEditor editor;
  final Map<FormEntry, Accessor> entries2accs;
  final Map<FormEntry, Mutator> entries2muts;


  CabalFormSection( final IFormPage page,
                    final Composite parent,
                    final CabalFormEditor editor,
                    final String text ) {
    super( parent,
           page.getManagedForm().getToolkit(),
           Section.DESCRIPTION | ExpandableComposite.TITLE_BAR );
    this.editor = editor;
    entries2accs = new HashMap<FormEntry, Accessor>();
    entries2muts = new HashMap<FormEntry, Mutator>();
    initialize( page.getManagedForm() );
    getSection().clientVerticalSpacing = 6;
    getSection().setData( "part", this );

    getSection().setText( text );
    createClient( page.getManagedForm().getToolkit() );
    mapData();
    fillInValues( true );
    editor.getModel().addDocumentListener( new IDocumentListener() {

      public void documentAboutToBeChanged( final DocumentEvent event ) {
        // unused
      }

      public void documentChanged( final DocumentEvent event ) {
        fillInValues( false );
      }
    } );
  }

  abstract void mapData();
  abstract void createClient( FormToolkit toolkit );

  IStatusLineManager getStatusLineManager() {
    return editor.getEditorSite().getActionBars().getStatusLineManager();
  }

  IManipulateCabalFile getManipulator() {
    CohatoeServer server = CohatoeServer.getInstance();
    return server.createFunction( IManipulateCabalFile.class );
  }

  FormEntry createFormEntry( final FormToolkit toolkit,
                             final Composite container,
                             final String label ) {
    FormEntry result = new FormEntry( container, toolkit, label, null, false );
    result.setFormEntryListener( createFormEntryListener() );
    return result;
  }

  // helping functions
  ////////////////////

  private void fillInValues( final boolean first ) {
    try {
      setAllEditable( true );
      getStatusLineManager().setErrorMessage( null );
      for( FormEntry entry: entries2accs.keySet() ) {
        fillValue( entry, entries2accs.get( entry ), first );
      }
    } catch( final InvalidCabalFileException icfex ) {
      setAllEditable( false );
      getStatusLineManager().setErrorMessage( icfex.getMessage() );
    } catch( final CoreException cex ) {
      HaskellUIPlugin.log( cex );
    }
  }

  private void fillValue( final FormEntry entry,
                          final Accessor acc,
                          final boolean first ) throws CoreException {
    String buffer = editor.getModel().get();
    IManipulateCabalFile manipulator = getManipulator();
    if( manipulator != null ) {
      String value = manipulator.get( buffer, acc );
      entry.setValue( value, first );
    }
  }

  private void setAllEditable( final boolean editable ) {
    for( FormEntry entry: entries2accs.keySet() ) {
      entry.setEditable( editable );
    }
  }

  private void setNewValue( final FormEntry text, final Mutator mutator ) {
    try {
      String newValue = text.getValue();
      IManipulateCabalFile manipulator = getManipulator();
      String buffer = editor.getModel().get();
      editor.getModel().set( manipulator.set( buffer, mutator, newValue ) );
    } catch( final CoreException cex ) {
      HaskellUIPlugin.log( cex );
    }
  }

  private IFormEntryListener createFormEntryListener() {
    return new FormEntryAdapter() {
      @Override
      public void textValueChanged( final FormEntry formEntry ) {
        setNewValue( formEntry, entries2muts.get( formEntry ) );
      }
    };
  }

}
