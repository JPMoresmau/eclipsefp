// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.text;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.preferences.editor.IEditorPreferenceNames;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultIndentLineAutoEditStrategy;
import org.eclipse.jface.text.DefaultLineTracker;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ILineTracker;
import org.eclipse.jface.text.IRegion;


/** <p>the auto indent strategy for the Haskell editor (insert spaces for
  * tabs, etc.).</p>
  *
  * @author Leif Frenzel
  */
public class HaskellAutoIndentStrategy extends DefaultIndentLineAutoEditStrategy {

  // interface methods of IAutoIndentStrategy
  ///////////////////////////////////////////

  @Override
  public void customizeDocumentCommand( final IDocument document,
                                        final DocumentCommand command ) {
    super.customizeDocumentCommand( document, command );
    if( isSpacesForTabs() ) {
      convertTabs( document, command );
    }
  }


  // helping methods
  //////////////////

  private void convertTabs( final IDocument document,
                            final DocumentCommand command ) {
    int index = command.text.indexOf( '\t' );
    if( index != -1 ) {
      StringBuffer buffer = new StringBuffer();
      ILineTracker lineTracker = createLineTracker( command );
      int lines = lineTracker.getNumberOfLines();
      try {
        for( int i = 0; i < lines; i++ ) {
          String line = getLine( command, lineTracker, i );
          int position = 0;
          if( i == 0 ) {
            position = getFirstPosition( document, command );
          }
          int length = line.length();
          for( int j = 0; j < length; j++ ) {
            char ch = line.charAt( j );
            if( ch == '\t' ) {
              position += insertTabSpaces( buffer, position );
            } else {
              buffer.append( ch );
              position++;
            }
          }
        }
        command.text = buffer.toString();
      } catch( BadLocationException e ) {
        // ignored
      }
    }
  }

  private int getFirstPosition( final IDocument document,
                                final DocumentCommand command )
                                                   throws BadLocationException {
    int cmdOffs = command.offset;
    IRegion firstLine = document.getLineInformationOfOffset( cmdOffs );
    return cmdOffs - firstLine.getOffset();
  }


  private ILineTracker createLineTracker( final DocumentCommand command ) {
    ILineTracker lineTracker = new DefaultLineTracker();
    lineTracker.set( command.text );
    return lineTracker;
  }


  private int insertTabSpaces( final StringBuffer buffer, final int position ) {
    int tabWidth = getTabWidth();
    int remainder = position % tabWidth;
    remainder = tabWidth - remainder;
    for( int k = 0; k < remainder; k++ ) {
      buffer.append( ' ' );
    }
    return remainder;
  }


  private String getLine( final DocumentCommand command,
                          final ILineTracker lineTracker,
                          final int lineNum ) throws BadLocationException {
    int offset = lineTracker.getLineOffset( lineNum );
    int endOffset = offset + lineTracker.getLineLength( lineNum );
    String line = command.text.substring( offset, endOffset );
    return line;
  }


  private int getTabWidth() {
    String key = IEditorPreferenceNames.EDITOR_TAB_WIDTH;
    return getPreferenceStore().getInt( key );
  }

  private boolean isSpacesForTabs() {
    String key = IEditorPreferenceNames.EDITOR_SPACES_FOR_TABS;
    return getPreferenceStore().getBoolean( key );
  }

  private IPreferenceStore getPreferenceStore() {
    return HaskellUIPlugin.getDefault().getPreferenceStore();
  }
}