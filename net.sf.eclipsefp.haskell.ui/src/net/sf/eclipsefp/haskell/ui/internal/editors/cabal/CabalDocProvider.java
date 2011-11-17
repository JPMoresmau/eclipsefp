// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.text.CabalPartitionScanner;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.eclipse.ui.editors.text.FileDocumentProvider;

/** <p>document provider for <code>.cabal</code> files.</p>
  *
  * <p>Note: this class is declared in <code>plugin.xml</code>.</p>
  *
  * @author Leif Frenzel
  */
public class CabalDocProvider extends FileDocumentProvider {

  public static final String COMMENT_CONTENT_TYPE = "__comment"; //$NON-NLS-1$

  public static final String[] TOKEN_TYPES = new String[] {
    IDocument.DEFAULT_CONTENT_TYPE,
    COMMENT_CONTENT_TYPE
  };


  // interface methods of IDocumentProvider
  /////////////////////////////////////////

  @Override
  protected IDocument createDocument( final Object elem ) throws CoreException {
    IDocument result = super.createDocument( elem );
    if( result != null ) {
      IDocumentPartitioner partitioner = createDocumentPartitioner();
      partitioner.connect( result );
      result.setDocumentPartitioner( partitioner );
    }
    return result;
  }


  // helping methods
  //////////////////

  private IDocumentPartitioner createDocumentPartitioner() {
    IPartitionTokenScanner partitionScanner = new CabalPartitionScanner();
    return new FastPartitioner( partitionScanner, TOKEN_TYPES );
  }
}
