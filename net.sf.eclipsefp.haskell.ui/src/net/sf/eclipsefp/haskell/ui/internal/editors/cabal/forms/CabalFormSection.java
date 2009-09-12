// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalFormEditor;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.SectionPart;
import org.eclipse.ui.forms.editor.IFormPage;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;

/** <p>form section super class - encapsulates some common functionality.</p>
  *
  * @author Leif Frenzel
  */
abstract class CabalFormSection extends SectionPart {

  final CabalFormEditor editor;
  final Map<FormEntry, CabalSyntax> entries2accs;
  private PackageDescriptionStanza stanza=null;


  CabalFormSection( final IFormPage page,
                    final Composite parent,
                    final CabalFormEditor editor,
                    final String text ) {
    super( parent,
           page.getManagedForm().getToolkit(),
           Section.DESCRIPTION | ExpandableComposite.TITLE_BAR );
    this.editor = editor;
    entries2accs = new HashMap<FormEntry, CabalSyntax>();
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



  public PackageDescriptionStanza getStanza() {
    return stanza;
  }

  public void setStanza( final PackageDescriptionStanza stanza ) {
    this.stanza = stanza;
    fillInValues( false );
  }

  IStatusLineManager getStatusLineManager() {
    return editor.getEditorSite().getActionBars().getStatusLineManager();
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
    //try {
      setAllEditable( true );
      getStatusLineManager().setErrorMessage( null );
      for( FormEntry entry: entries2accs.keySet() ) {
        fillValue( entry, entries2accs.get( entry ), first );
      }
    /*} catch( final InvalidCabalFileException icfex ) {
      setAllEditable( false );
      getStatusLineManager().setErrorMessage( icfex.getMessage() );
    } catch( final CoreException cex ) {
      HaskellUIPlugin.log( cex );
    }*/
  }

  private void fillValue( final FormEntry entry,
                          final CabalSyntax acc,
                          final boolean first ) {
    /*String buffer = editor.getModel().get();

    IManipulateCabalFile manipulator = getManipulator();
    if( manipulator != null ) {
      String value = manipulator.get( buffer, acc );
      entry.setValue( value, first );
    }*/
    /*if (acc==Accessor.GET_NAME){
      PackageDescription pd=PackageDescriptionLoader.load( buffer );
      for (PackageDescriptionStanza pds:pd.getStanzas()){
        if (pds instanceof GeneralStanza){
          entry.setValue( pds.getName(), first );
        }
      }
    }*/
    if(stanza!=null){
      String value=stanza.getProperties().get( acc );
      entry.setValue( value,first );
    } else {
      entry.setValue( "" ,first);
    }
  }

  private void setAllEditable( final boolean editable ) {
    for( FormEntry entry: entries2accs.keySet() ) {
      entry.setEditable( editable );
    }
  }

  private void setNewValue( final FormEntry text, final CabalSyntax mutator ) {
    //try {
      String newValue = text.getValue();
      //IManipulateCabalFile manipulator = getManipulator();
      //String buffer = editor.getModel().get();
      //editor.getModel().set( manipulator.set( buffer, mutator, newValue ) );
      stanza.getProperties().put( mutator.getCabalName(), newValue );

      /*String realValue=stanza.getRealValue( mutator, newValue );
      ValuePosition vp=stanza.getPositions().get( mutator );
      if (vp==null){
        vp=new ValuePosition(stanza.getEndLine(),stanza.getEndLine(),stanza.getIndent());
      }*/
      RealValuePosition vp=stanza.update( mutator, newValue );

      IDocument doc=editor.getModel();
      try {
        int st=doc.getLineOffset( vp.getStartLine() )+vp.getInitialIndent();
        int end=doc.getLineOffset( vp.getEndLine() );

        doc.replace( st, end-st,vp.getRealValue() );
      } catch(BadLocationException ble){
        ble.printStackTrace();
      }

  }

  private IFormEntryListener createFormEntryListener() {
    return new FormEntryAdapter() {
      @Override
      public void textValueChanged( final FormEntry formEntry ) {
        setNewValue( formEntry, entries2accs.get( formEntry ) );
      }
    };
  }

}
