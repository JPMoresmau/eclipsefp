// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.DefaultPartitionScanner;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.HaskellDocumentPartitioner;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ScannerManager;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ScionTokenScanner;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.editors.text.FileDocumentProvider;
import org.eclipse.ui.part.FileEditorInput;

/** <p>The HaskellDocumentProvides knows how to create a Haskell document
  * (a model for the editor) from a file resource.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellDocumentProvider extends FileDocumentProvider {


  public static IDocumentPartitioner createDocumentPartitioner() {
    return new FastPartitioner( new DefaultPartitionScanner(), new String[]{IDocument.DEFAULT_CONTENT_TYPE });
  }

  public static void connectToPartitioner( final Object element,
                                           final IDocument document )
                                                         {
    IDocumentPartitioner partitioner = getPartitioner( element );
    partitioner.connect( document );
    document.setDocumentPartitioner( partitioner );
  }


  // interface methods of IDocumentProvider
  /////////////////////////////////////////

  @Override
  protected IDocument createDocument( final Object elem ) throws CoreException {
    IDocument result = super.createDocument( elem );
    if( result != null ) {
      connectToPartitioner( elem, result );
    }
    return result;
  }

  @Override
    protected IDocument createEmptyDocument() {
      SynchronizableDocument sd= new SynchronizableDocument();
      return sd;
    }

  @Override
  protected IAnnotationModel createAnnotationModel( final Object element )
                                                          throws CoreException {
    IAnnotationModel result = null;
    if( element instanceof IFile ) {
      result = new HaskellAnnotationModel( ( IResource )element );
    } else if( element instanceof IFileEditorInput ) {
      IFileEditorInput editorInput = ( IFileEditorInput )element;
      IFile file = editorInput.getFile();
      result = new HaskellAnnotationModel( file );
    } else {
      result = super.createAnnotationModel( element );
    }
    return result;
  }


  // helping methods
  //////////////////

  private static IDocumentPartitioner getPartitioner( final Object elem ){

    IFile file=((FileEditorInput)elem).getFile();

    ScionTokenScanner partitionScanner = new ScionTokenScanner( ScannerManager.getInstance(), file );
    return new HaskellDocumentPartitioner( partitionScanner, new String[]{IDocument.DEFAULT_CONTENT_TYPE,HaskellEditor.TEXT_CONTENTTYPE} );
  }
}