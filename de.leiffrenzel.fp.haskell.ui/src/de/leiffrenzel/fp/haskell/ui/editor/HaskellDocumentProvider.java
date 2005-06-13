// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.editor;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.*;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.editors.text.FileDocumentProvider;

import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;
import de.leiffrenzel.fp.haskell.ui.editor.text.HaskellPartitionScanner;
import de.leiffrenzel.fp.haskell.ui.editor.text.LiterateHaskellPartitionScanner;

/** <p>The HaskellDocumentProvides knows how to create a Haskell document 
  * (a model for the editor) from a file resource.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellDocumentProvider extends FileDocumentProvider {

  private static final String[] TOKEN_TYPES = new String[] { 
    IDocument.DEFAULT_CONTENT_TYPE,
    IPartitionTypes.HS_COMMENT, 
    IPartitionTypes.HS_LITERATE_COMMENT
  };

  public static IDocumentPartitioner createDocumentPartitioner() {
    IPartitionTokenScanner partitionScanner = new HaskellPartitionScanner();
    return new FastPartitioner( partitionScanner, TOKEN_TYPES );
  }
  
  protected IDocument createDocument( final Object elem ) throws CoreException {
    IDocument result = super.createDocument( elem );
    if( result != null ) {
      IDocumentPartitioner partitioner = getPartitioner( elem ); 
      partitioner.connect( result );
      result.setDocumentPartitioner( partitioner );
    }
    return result;
  }
  
  
  // helping methods
  //////////////////
  
  private IDocumentPartitioner getPartitioner( final Object elem ) {
    IPartitionTokenScanner partitionScanner;
    if( isLiterate( elem ) ) {
      partitionScanner = new LiterateHaskellPartitionScanner();
    } else {
      partitionScanner = new HaskellPartitionScanner();
    }
    return new FastPartitioner( partitionScanner, TOKEN_TYPES );
  }

  private boolean isLiterate( final Object elem ) {
    boolean isLiterate = false;
    if( elem instanceof IFileEditorInput ) {
      IFile input = ( ( IFileEditorInput )elem ).getFile(); 
      String literateExt = ResourceUtil.EXTENSION_LHS;
      isLiterate = input.getFileExtension().equals( literateExt );
    }
    return isLiterate;
  }
}