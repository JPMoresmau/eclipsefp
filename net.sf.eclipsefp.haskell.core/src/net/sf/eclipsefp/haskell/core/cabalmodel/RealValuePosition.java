package net.sf.eclipsefp.haskell.core.cabalmodel;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

/**
 *
 * @author JP Moresmau
 *
 */
public class RealValuePosition extends ValuePosition {
  private final String realValue;

  public RealValuePosition(final ValuePosition pos,final String realValue) {
    super( pos.getStartLine(), pos.getEndLine(), pos.getInitialIndent() );
    this.realValue=realValue;
 }

  public String getRealValue() {
    return realValue;
  }

  @Override
  public String toString() {
   return realValue+" "+ super.toString(); //$NON-NLS-1$
  }

  public void updateDocument(final IDocument doc){
    try {
      int st=doc.getLineOffset( getStartLine() )+getInitialIndent();
      int end=doc.getLineOffset( getEndLine() );
      if (end-st>0){
        doc.replace( st, end-st,getRealValue() );
      }
    } catch(BadLocationException ble){
      System.out.println( this);
      ble.printStackTrace();
    }
  }
}
