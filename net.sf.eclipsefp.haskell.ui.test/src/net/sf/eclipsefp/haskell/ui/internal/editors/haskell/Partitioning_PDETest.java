// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import java.io.ByteArrayInputStream;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.ui.part.FileEditorInput;

public class Partitioning_PDETest extends TestCaseWithProject {

  public void testEmpty() throws Exception {
    IDocument document = mkDoc ( "\n" );
    asserPartitionType( document, 0, IDocument.DEFAULT_CONTENT_TYPE );
  }


  // single-line comment partition
  ////////////////////////////////

  public void testSLCEmpty() throws Exception {
    IDocument document = mkDoc ( "--\n" );
    ITypedRegion partition = document.getPartition( 0 );
    assertEquals( IPartitionTypes.HS_COMMENT, partition.getType() );
  }

  public void testSLC() throws Exception {
    IDocument document = mkDoc ( "a--b\n" );
    asserPartitionType( document, 0, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 1, IPartitionTypes.HS_COMMENT );
  }

  public void testSLCFollowedByLine() throws Exception {
    IDocument document = mkDoc ( "--a\\\nb\nc" );
    asserPartitionType( document, 4, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IDocument.DEFAULT_CONTENT_TYPE );
  }

  // [1838106][Editor] Wrong coloring of comments in string literals
  public void testSLCInStringLiteral() throws Exception {
    IDocument document = mkDoc ( "main = putStrLn \"-- a\"\n" );
    asserPartitionType( document, 17, IPartitionTypes.HS_STRING );
  }


  // multi-line comment partition
  ///////////////////////////////

  public void testMLCOnly() throws Exception {
    IDocument document = mkDoc ( "{-- a\nb --}\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 11, IDocument.DEFAULT_CONTENT_TYPE );
  }

  public void testMLCBeg() throws Exception {
    IDocument document = mkDoc ( "{-- a\nb --}\nmodule A where\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 11, IDocument.DEFAULT_CONTENT_TYPE );
  }

  public void testMLCEnd() throws Exception {
    IDocument document = mkDoc ( "module A where\n{-- a\nb --}\n" );
    asserPartitionType( document, 0, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 15, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 20, IPartitionTypes.HS_COMMENT );
  }

  public void testMLCBetween() throws Exception {
    String content = "{-- a\nb --}\nmodule A where\n{-- b\nc --}\n";
    IDocument document = mkDoc ( content );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 12, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 27, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 33, IPartitionTypes.HS_COMMENT );
  }

  public void testMLCSingleLine() throws Exception {
    IDocument document = mkDoc ( "{-- a --}\nmodule A where\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 10, IDocument.DEFAULT_CONTENT_TYPE );
  }

  public void testMLCEmpty() throws Exception {
    IDocument document = mkDoc ( "{----}\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 3, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 5, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IDocument.DEFAULT_CONTENT_TYPE );
  }

  public void testMLCIncludingSLC() throws Exception {
    IDocument document = mkDoc ( "{-- a\n--b\nc --}\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 6, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 10, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 15, IDocument.DEFAULT_CONTENT_TYPE );
  }

  // [1838106][Editor] Wrong coloring of comments in string literals
  public void testMLCInStringLiteral() throws Exception {
    IDocument document = mkDoc ( "main = putStrLn \"{-- a --}\"\n" );
    asserPartitionType( document, 17, IPartitionTypes.HS_STRING );
  }

  // TODO lf breaks, reported in
  // [ 1838099 ] [Editor] Syntax-highlighting for nested multi-line comments
  public void testNestedMLC() throws Exception {
    IDocument document = mkDoc ( "{-- a\nb {-- c --}\n--}\n" );
    asserPartitionType( document, 0, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 8, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 16, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 17, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 20, IPartitionTypes.HS_COMMENT );
    asserPartitionType( document, 21, IDocument.DEFAULT_CONTENT_TYPE );
  }

  // [ 1837352 ] [Editor] Highlighting quotations inside apostrophies
  public void testQuotesInSingleQuotes() throws Exception {
    IDocument document = mkDoc ( "putStrLn ['\"', '\"']\n" );
    asserPartitionType( document, 9, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 10, IPartitionTypes.HS_CHARACTER );
    asserPartitionType( document, 12, IPartitionTypes.HS_CHARACTER );
    asserPartitionType( document, 13, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 15, IPartitionTypes.HS_CHARACTER );
    asserPartitionType( document, 18, IDocument.DEFAULT_CONTENT_TYPE );
  }

  public void testSingleQuotesInQuotes() throws Exception {
    IDocument document = mkDoc ( "putStrLn [\"'\", \"'\"]\n" );
    asserPartitionType( document, 9, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 10, IPartitionTypes.HS_STRING );
    asserPartitionType( document, 12, IPartitionTypes.HS_STRING );
    asserPartitionType( document, 13, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 15, IPartitionTypes.HS_STRING );
    asserPartitionType( document, 18, IDocument.DEFAULT_CONTENT_TYPE );
  }

  // literate-style comments
  //////////////////////////

  public void testTraditionalLiterateComments() throws Exception {
    String content = "bla\n> module Demo2 where\nbla\n";
    IDocument document = mkLiterateDoc( content );
    asserPartitionType( document, 0, IPartitionTypes.HS_LITERATE_COMMENT );
    asserPartitionType( document, 4, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 25, IPartitionTypes.HS_LITERATE_COMMENT );
  }

  public void testLaTeXLiterateComments() throws Exception {
    String content = "a\\begin{code}\nmodule Demo where\n\\end{code}\nBla";
    IDocument document = mkLiterateDoc( content );
    // note: we must count the begin and end latex sequences as 'code',
    // instead as literate comment, because I haven't found a way in JFace's
    // rules for returning a partition type for a pattern _between_ start/end,
    // but excluding the start/end sequences themselves
    //
    // The code scanner will take care of these two special sequences
    asserPartitionType( document, 0, IPartitionTypes.HS_LITERATE_COMMENT );
    asserPartitionType( document, 1, IDocument.DEFAULT_CONTENT_TYPE );
    asserPartitionType( document, 43, IPartitionTypes.HS_LITERATE_COMMENT );
  }


  // helping functions
  ////////////////////

  private IDocument mkDoc ( final String input ) throws CoreException {
    return createDocument( input, ResourceUtil.EXTENSION_HS );
  }

  private IDocument mkLiterateDoc( final String input ) throws CoreException {
    return createDocument( input, ResourceUtil.EXTENSION_LHS );
  }

  private IDocument createDocument( final String input,
                                    final String ext ) throws CoreException {
    IDocument result = new Document( input );
    IFile file = project.getFile( "a." + ext );
    file.create( new ByteArrayInputStream( input.getBytes() ), true, null );
    FileEditorInput fei = new FileEditorInput( file );
    HaskellDocumentProvider.connectToPartitioner( fei, result );
    return result;
  }

  private void asserPartitionType( final IDocument document,
                                   final int pos,
                                   final String type ) throws Exception {
    ITypedRegion partition = document.getPartition( pos );
    assertEquals( type, partition.getType() );
  }
}
