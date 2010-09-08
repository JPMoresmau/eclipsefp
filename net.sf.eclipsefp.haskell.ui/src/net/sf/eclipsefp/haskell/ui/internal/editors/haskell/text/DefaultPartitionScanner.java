package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;

/**
 * <p>A degenerate partition scanner that always return the default content type, since we do not do any lexing of Haskell ourselves</p>
  *
  * @author JP Moresmau
 */
public class DefaultPartitionScanner implements IPartitionTokenScanner {

  private int offset;
  private int length;
  private boolean has=true;

  public void setPartialRange( final IDocument document, final int offset, final int length,
      final String contentType, final int partitionOffset ) {
    setRange(document,offset,length);
  }

  public int getTokenLength() {
   return length;
  }

  public int getTokenOffset() {
   return offset;
  }

  public IToken nextToken() {
    if (has){
      has=false;
      return new Token(IDocument.DEFAULT_CONTENT_TYPE);
    }
    return Token.EOF;
  }

  public void setRange( final IDocument document, final int offset, final int length ) {
   this.offset=offset;
   this.length=length;
  }

}
