// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.presentation.IPresentationDamager;
import org.eclipse.jface.text.presentation.IPresentationRepairer;
import org.eclipse.swt.custom.StyleRange;


/** <p>simple damager and repairer that uses no scanner rules, but knows
  * how to handle multi-line text regions.</p>
  *
  * @author Leif Frenzel
  */
public class NonRuleBasedDamagerRepairer implements IPresentationDamager,
                                                    IPresentationRepairer,
                                                    IEditorPreferenceNames {

  /** The document this object works on */
  protected IDocument document;
  private final boolean literate;

  public NonRuleBasedDamagerRepairer( final boolean literate ) {
    this.literate = literate;
  }

  public void setDocument( final IDocument document ) {
    this.document = document;
  }

  /**
   * Returns the end offset of the line that contains the specified offset or
   * if the offset is inside a line delimiter, the end offset of the next line.
   *
   * @param offset the offset whose line end offset must be computed
   * @return the line end offset for the given offset
   * @exception BadLocationException if offset is invalid in the
   *                                 current document
   */
  protected int endOfLineOf( final int offset ) throws BadLocationException {
    int result;
    IRegion info = document.getLineInformationOfOffset( offset );
    if( offset <= info.getOffset() + info.getLength() ) {
      result = info.getOffset() + info.getLength();
    } else {
      int line = document.getLineOfOffset( offset );
      try {
        info = document.getLineInformation( line + 1 );
        result = info.getOffset() + info.getLength();
      } catch( BadLocationException x ) {
        result = document.getLength();
      }
    }
    return result;
  }

  public IRegion getDamageRegion( final ITypedRegion partition,
                                  final DocumentEvent event,
                                  final boolean documentPartitioningChanged ) {
    IRegion result = partition;
    if( !documentPartitioningChanged ) {
      try {
        IRegion info = document.getLineInformationOfOffset( event.getOffset() );
        int start = Math.max( partition.getOffset(), info.getOffset() );
        int end = event.getOffset() + getEventLength( event );

        if(    info.getOffset() <= end
            && end <= info.getOffset() + info.getLength() ) {
          // optimize the case of the same line
          end = info.getOffset() + info.getLength();
        } else {
          end = endOfLineOf( end );
        }
        end = Math.min( partition.getOffset() + partition.getLength(), end );
        result = new Region( start, end - start );
      } catch( BadLocationException x ) {
        // ignored
      }
    }
    return result;
  }

  public void createPresentation( final TextPresentation presentation,
                                  final ITypedRegion region ) {
    int offset = region.getOffset();
    int length = region.getLength();
    addRange( presentation, offset, length, getDefaultAttribute() );
  }

  /**
   * Adds style information to the given text presentation.
   *
   * @param presentation the text presentation to be extended
   * @param offset       the offset of the range to be styled
   * @param length       the length of the range to be styled
   * @param attribute    the attribute describing the style of the range to
   *                     be styled
   */
  protected void addRange( final TextPresentation presentation,
                           final int offset,
                           final int length,
                           final TextAttribute attribute ) {
    if( attribute != null ) {
      presentation.addStyleRange( new StyleRange( offset,
                                                  length,
                                                  attribute.getForeground(),
                                                  attribute.getBackground(),
                                                  attribute.getStyle() ) );
    }
  }

  // helping methods
  //////////////////

  private TextAttribute getDefaultAttribute() {
    TextAttribute result;
    if( literate ) {
      result = ScannerManager.getInstance().getLiterateCommentAttribute();
    } else {
      result = ScannerManager.getInstance().getCommentAttribute();
    }
    return result;
  }

  private int getEventLength( final DocumentEvent event ) {
    return event.getText() == null ? event.getLength()
                                   : event.getText().length();
  }
}