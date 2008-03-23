//Borrowed code from PyDev
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * Action to move to the first character on the line or to the line start
 * depending on the cursor position.
 *
 * @author Fabio Zadrozny
 * @author Andriy Palamarchuk
 *
 * @author Leif Frenzel (changed formatting and minor refactorings)
 */
public abstract class AbstractAction implements IEditorActionDelegate {

  private IEditorPart targetEditor;

  /*
   * Beeps on exception.
   */
  protected static void beep( final Exception ex ) {
    Display.getDefault().beep();
    ex.printStackTrace();
  }

  /**
   * This method returns the delimiter for the document
   *
   * @param doc
   * @param startLineIndex
   * @return delimiter for the document (\n|\r\|r\n)
   */
  public static String getDelimiter( final IDocument doc,
                                     final int startLineIndex )
                                                   throws BadLocationException {
    String endLineDelim = doc.getLineDelimiter( startLineIndex );
    if( endLineDelim == null ) {
      endLineDelim = doc.getLegalLineDelimiters()[ 0 ];
    }
    return endLineDelim;
  }

  public void selectionChanged( final IAction action,
                                final ISelection selection ) {
    // unused
  }

  /**
   * This function returns the text editor.
   */
  protected ITextEditor getTextEditor() {
    if( targetEditor instanceof ITextEditor ) {
      return ( ITextEditor )targetEditor;
    }
    throw new AssertionError(   "Expecting text editor. Found:"
                              + targetEditor.getClass().getName() );
  }

  /**
   * Run to the first char (other than whitespaces) or to the real first char.
   */
  public void setActiveEditor( final IAction action,
                               final IEditorPart targetEditor ) {
    this.targetEditor = targetEditor;
  }

  /**
   * Returns the position of the last non whitespace char in the current line.
   *
   * @param doc
   * @param cursorOffset
   * @return position of the last character of the line (returned as an
   *         absolute offset)
   *
   * @throws BadLocationException
   */
  protected int getLastCharPosition( final IDocument doc,
                                     final int cursorOffset )
                                                   throws BadLocationException {
    IRegion region = doc.getLineInformationOfOffset( cursorOffset );
    int offset = region.getOffset();
    String src = doc.get( offset, region.getLength() );

    int i = src.length();
    boolean breaked = false;
    while( i > 0 ) {
      i--;
      // we have to break if we find a character that is not a whitespace or a
      // tab.
      if(    Character.isWhitespace( src.charAt( i ) ) == false
          && src.charAt( i ) != '\t' ) {
        breaked = true;
        break;
      }
    }
    if( !breaked ) {
      i--;
    }
    return offset + i;
  }

  protected static int getFirstCharRelativePosition( final IDocument document,
                                                     final int cursorOffset )
                                                   throws BadLocationException {
    IRegion lineInfo = document.getLineInformationOfOffset( cursorOffset );
    return getFirstCharRelativePosition( document, lineInfo );
  }

  public static int getFirstCharRelativePosition( final IDocument document,
                                                  final IRegion region )
                                                   throws BadLocationException {
    String src = document.get( region.getOffset(), region.getLength() );

    int i = 0;
    boolean breaked = false;
    while( i < src.length() ) {
      if( Character.isWhitespace( src.charAt( i ) ) == false
          && src.charAt( i ) != '\t' ) {
        i++;
        breaked = true;
        break;
      }
      i++;
    }
    if( !breaked ) {
      i++;
    }
    return i - 1;
  }
}