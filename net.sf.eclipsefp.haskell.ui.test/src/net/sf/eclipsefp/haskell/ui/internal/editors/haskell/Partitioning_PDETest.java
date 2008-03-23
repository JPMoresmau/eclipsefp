// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import junit.framework.TestCase;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITypedRegion;

public class Partitioning_PDETest extends TestCase {

  public void testEmpty() throws Exception {
    IDocument document = createDocument( "\n" );
    asserPartitionType( document, 0, IDocument.DEFAULT_CONTENT_TYPE );
  }


  // single-line comment partition
  ////////////////////////////////

  public void testSLCEmpty() throws Exception {
    IDocument document = createDocument( "--\n" );
    ITypedRegion partition = document.getPartition( 0 );
    assertEquals( IPartitionTypes.HS_COMMENT, partition.getType() );
  }

  public void testSLC() throws Exception {
    IDocument document = createDocument( "a--b\n" );
    asserPartitionType( document, 0, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 1, IPartitionTypes.HS_COMMENT );
  }

  public void testSLCFollowedByLine() throws Exception {
    IDocument document = createDocument( "--a\\\nb\nc" );
    asserPartitionType( document, 4, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IDocument.DEFAULT_CONTENT_TYPE );
  }

  // TODO lf breaks, reported in
  // [1838106][Editor] Wrong coloring of comments in string literals
  public void testSLCInStringLiteral() throws Exception {
    IDocument document = createDocument( "main = putStrLn \"-- a\"\n" );
    asserPartitionType( document, 17, IDocument.DEFAULT_CONTENT_TYPE );
  }


  // multi-line comment partition
  ///////////////////////////////

  public void testMLCOnly() throws Exception {
    IDocument document = createDocument( "{-- a\nb --}\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 11, IDocument.DEFAULT_CONTENT_TYPE );
  }

  public void testMLCBeg() throws Exception {
    IDocument document = createDocument( "{-- a\nb --}\nmodule A where\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 11, IDocument.DEFAULT_CONTENT_TYPE );
  }

  public void testMLCEnd() throws Exception {
    IDocument document = createDocument( "module A where\n{-- a\nb --}\n" );
    asserPartitionType( document, 0, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 15, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 20, IPartitionTypes.HS_COMMENT );
  }

  public void testMLCBetween() throws Exception {
    String content = "{-- a\nb --}\nmodule A where\n{-- b\nc --}\n";
    IDocument document = createDocument( content );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 12, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 27, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 33, IPartitionTypes.HS_COMMENT );
  }

  public void testMLCSingleLine() throws Exception {
    IDocument document = createDocument( "{-- a --}\nmodule A where\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 10, IDocument.DEFAULT_CONTENT_TYPE );
  }

  public void testMLCEmpty() throws Exception {
    IDocument document = createDocument( "{----}\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 3, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 5, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IDocument.DEFAULT_CONTENT_TYPE );
  }

  public void testMLCIncludingSLC() throws Exception {
    IDocument document = createDocument( "{-- a\n--b\nc --}\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 10, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 15, IDocument.DEFAULT_CONTENT_TYPE );
  }

  // TODO lf breaks, reported in
  // [1838106][Editor] Wrong coloring of comments in string literals
  public void testMLCInStringLiteral() throws Exception {
    IDocument document = createDocument( "main = putStrLn \"{-- a --}\"\n" );
    asserPartitionType( document, 17, IDocument.DEFAULT_CONTENT_TYPE );
  }

  // TODO lf breaks, reported in
  // [ 1838099 ] [Editor] Syntax-highlighting for nested multi-line comments
  public void testNestedMLC() throws Exception {
    IDocument document = createDocument( "{-- a\nb {-- c --}\n--}\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 8, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 16, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 17, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 20, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 21, IDocument.DEFAULT_CONTENT_TYPE );
  }


  // helping functions
  ////////////////////

  private IDocument createDocument( final String input ) {
    IDocument result = new Document( input );
    HaskellDocumentProvider.connectToPartitioner( null, result );
    return result;
  }

  private void asserPartitionType( final IDocument document,
                                   final int pos,
                                   final String type ) throws Exception {
    ITypedRegion partition = document.getPartition( pos );
    assertEquals( type, partition.getType() );
  }
}
