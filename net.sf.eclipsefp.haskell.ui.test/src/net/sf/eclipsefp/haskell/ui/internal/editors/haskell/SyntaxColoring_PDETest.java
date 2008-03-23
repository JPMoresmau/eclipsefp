// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ColorProvider;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellCodeScanner;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.swt.graphics.RGB;

public class SyntaxColoring_PDETest extends TestCase {

  public void testDefaultColor() {
    IDocument document = createDocument( "some text\n" );
    ITokenScanner scanner = new HaskellCodeScanner();
    scanner.setRange( document, 0, 1 );
    IToken token = scanner.nextToken();
    assertColor( token, ColorProvider.DEFAULT_OTHER );
  }

  public void testKeywordColor() {
    assertColor( tokenize( "module\n" ), ColorProvider.DEFAULT_KEYWORD );
    assertColor( tokenize( "import\n" ), ColorProvider.DEFAULT_KEYWORD );
  }

  public void testDummyKeywordColor() {
    assertColor( tokenize( " X\n" ), ColorProvider.DEFAULT_OTHER );
  }

  public void testQuotedQuote() {
    // see [1837352] the problem is that the first quotation char is taken to
    // be the beginning of a string literal that goes until the word 'test'
    // begins
    IDocument document = createDocument( "putStrLn ['\"', \"test\" ,'\"']\n" );
    ITokenScanner scanner = new HaskellCodeScanner();
    scanner.setRange( document, 0, document.getLength() );
    assertColor( scanner.nextToken(), ColorProvider.DEFAULT_FUNCTION );
    // TODO lf asserts (what do we expect here in terms of TextAttributes?
  }

  // helping methods
  //////////////////

  private void assertColor( final IToken token, final RGB color ) {
    TextAttribute ta = ( TextAttribute )token.getData();
    assertEquals( color, ta.getForeground().getRGB() );
  }

  private IDocument createDocument( final String input ) {
    IDocument result = new Document( input );
    HaskellDocumentProvider.connectToPartitioner( null, result );
    return result;
  }

  private IToken tokenize( final String keyword ) {
    IDocument document = createDocument( keyword );
    ITokenScanner scanner = new HaskellCodeScanner();
    int length = keyword.trim().length();
    int offset = keyword.indexOf( keyword.trim() );
    scanner.setRange( document, offset, length );
    return scanner.nextToken();
  }
}
