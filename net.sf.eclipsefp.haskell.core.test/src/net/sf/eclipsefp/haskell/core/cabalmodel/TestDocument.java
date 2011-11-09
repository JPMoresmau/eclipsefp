package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.BadPositionCategoryException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.IDocumentPartitioningListener;
import org.eclipse.jface.text.IPositionUpdater;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Position;

/**
 * naive implementation of a IDocument, sufficient for Cabal tests
 * @author JP Moresmau
 *
 */
public class TestDocument implements IDocument {
  private final StringBuilder content;

  public TestDocument(final String content) {
    this.content=new StringBuilder(content);
  }

  public void addDocumentListener( final IDocumentListener arg0 ) {
    // TODO Auto-generated method stub

  }

  public void addDocumentPartitioningListener(
      final IDocumentPartitioningListener arg0 ) {
    // TODO Auto-generated method stub

  }

  public void addPosition( final Position arg0 ) throws BadLocationException {
    // TODO Auto-generated method stub

  }

  public void addPosition( final String arg0, final Position arg1 )
      throws BadLocationException, BadPositionCategoryException {
    // TODO Auto-generated method stub

  }

  public void addPositionCategory( final String arg0 ) {
    // TODO Auto-generated method stub

  }

  public void addPositionUpdater( final IPositionUpdater arg0 ) {
    // TODO Auto-generated method stub

  }

  public void addPrenotifiedDocumentListener( final IDocumentListener arg0 ) {
    // TODO Auto-generated method stub

  }

  public int computeIndexInCategory( final String arg0, final int arg1 )
      throws BadLocationException, BadPositionCategoryException {
    // TODO Auto-generated method stub
    return 0;
  }

  public int computeNumberOfLines( final String arg0 ) {
    // TODO Auto-generated method stub
    return 0;
  }

  public ITypedRegion[] computePartitioning( final int arg0, final int arg1 )
      throws BadLocationException {
    // TODO Auto-generated method stub
    return null;
  }

  public boolean containsPosition( final String arg0, final int arg1, final int arg2 ) {
    // TODO Auto-generated method stub
    return false;
  }

  public boolean containsPositionCategory( final String arg0 ) {
    // TODO Auto-generated method stub
    return false;
  }

  public String get() {
    return content.toString();
  }

  public String get( final int offset, final int length ) throws BadLocationException {
    return content.substring( offset,offset+length);
  }

  public char getChar( final int arg0 ) throws BadLocationException {
    return content.charAt( arg0 );
  }

  public String getContentType( final int arg0 ) throws BadLocationException {
    // TODO Auto-generated method stub
    return null;
  }

  public IDocumentPartitioner getDocumentPartitioner() {
    // TODO Auto-generated method stub
    return null;
  }

  public String[] getLegalContentTypes() {
    // TODO Auto-generated method stub
    return null;
  }

  public String[] getLegalLineDelimiters() {
    // TODO Auto-generated method stub
    return null;
  }

  public int getLength() {
    return content.length();
  }

  public String getLineDelimiter( final int arg0 ) throws BadLocationException {
    // TODO Auto-generated method stub
    return null;
  }

  public IRegion getLineInformation( final int arg0 ) throws BadLocationException {
    // TODO Auto-generated method stub
    return null;
  }

  public IRegion getLineInformationOfOffset( final int arg0 )
      throws BadLocationException {
    // TODO Auto-generated method stub
    return null;
  }

  public int getLineLength( final int arg0 ) throws BadLocationException {
    // TODO Auto-generated method stub
    return 0;
  }

  public int getLineOfOffset( final int arg0 ) throws BadLocationException {
    // TODO Auto-generated method stub
    return 0;
  }

  public int getLineOffset( final int line ) throws BadLocationException {
    try {
      BufferedReader br=new BufferedReader( new StringReader( content.toString() ) );
      String lineContent=br.readLine();
      int count=0;
      int offset=0;
      while (line>count){
        count++;
        offset+=lineContent.length();
        offset+=PlatformUtil.NL.length();
        lineContent=br.readLine();
      }
      return offset;
    } catch (IOException ioe){
      ioe.printStackTrace();
    }
    return 0;
  }

  public int getNumberOfLines() {
    try {
      BufferedReader br=new BufferedReader( new StringReader( content.toString() ) );
      String lineContent=br.readLine();
      int count=0;
      while (lineContent!=null){
        count++;
        lineContent=br.readLine();
      }
      return count;
    } catch (IOException ioe){
      ioe.printStackTrace();
    }
    return 0;
  }

  public int getNumberOfLines( final int arg0, final int arg1 ) throws BadLocationException {
    // TODO Auto-generated method stub
    return 0;
  }

  public ITypedRegion getPartition( final int arg0 ) throws BadLocationException {
    // TODO Auto-generated method stub
    return null;
  }

  public String[] getPositionCategories() {
    // TODO Auto-generated method stub
    return null;
  }

  public IPositionUpdater[] getPositionUpdaters() {
    // TODO Auto-generated method stub
    return null;
  }

  public Position[] getPositions( final String arg0 )
      throws BadPositionCategoryException {
    // TODO Auto-generated method stub
    return null;
  }

  public void insertPositionUpdater( final IPositionUpdater arg0, final int arg1 ) {
    // TODO Auto-generated method stub

  }

  public void removeDocumentListener( final IDocumentListener arg0 ) {
    // TODO Auto-generated method stub

  }

  public void removeDocumentPartitioningListener(
      final IDocumentPartitioningListener arg0 ) {
    // TODO Auto-generated method stub

  }

  public void removePosition( final Position arg0 ) {
    // TODO Auto-generated method stub

  }

  public void removePosition( final String arg0, final Position arg1 )
      throws BadPositionCategoryException {
    // TODO Auto-generated method stub

  }

  public void removePositionCategory( final String arg0 )
      throws BadPositionCategoryException {
    // TODO Auto-generated method stub

  }

  public void removePositionUpdater( final IPositionUpdater arg0 ) {
    // TODO Auto-generated method stub

  }

  public void removePrenotifiedDocumentListener( final IDocumentListener arg0 ) {
    // TODO Auto-generated method stub

  }

  public void replace( final int offset, final int length, final String text )
      throws BadLocationException {
    content.replace( offset,offset+length,text);

  }

  public int search( final int arg0, final String arg1, final boolean arg2, final boolean arg3,
      final boolean arg4 ) throws BadLocationException {
    // TODO Auto-generated method stub
    return 0;
  }

  public void set( final String arg0 ) {
    // TODO Auto-generated method stub

  }

  public void setDocumentPartitioner( final IDocumentPartitioner arg0 ) {
    // TODO Auto-generated method stub

  }

}
