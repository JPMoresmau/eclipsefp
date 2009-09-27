// Copyright (c) 2004-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.eclipse.jface.text.reconciler.IReconcilingStrategyExtension;

/** <p> helper class that defines the model reconciling for the Haskell
  * editor.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellReconcilingStrategy implements IReconcilingStrategy,
                                                   IReconcilingStrategyExtension {

  private final HaskellEditor editor;
//  private ScionInstance instance=null;
//  private final HaskellFoldingStructureProvider foldingStructureProvider;

//  private MarkOccurrenceComputer markOccurrencesComputer;
//  private IDocument document;
//  private IFile file;

  public HaskellReconcilingStrategy( final HaskellEditor editor ) {
    this.editor = editor;
  /*  IEditorInput input=editor.getEditorInput();
    if( input != null && input instanceof IFileEditorInput ) {
      IFileEditorInput fei = ( IFileEditorInput )input;
      this.file = fei.getFile();
      if( file != null && file.exists() ) {
        instance=HaskellUIPlugin.getDefault().getScionInstanceManager( file );
      }
    }
    foldingStructureProvider = new HaskellFoldingStructureProvider( editor );*/
  }


  // interface methods of IReconcilingStrategy
  // //////////////////////////////////////////

  public void setDocument( final IDocument document ) {
   /* this.document = document;
    foldingStructureProvider.setDocument( document );
    if( markOccurrencesComputer != null ) {
      markOccurrencesComputer.setDocument( document );
    }*/
  }

  public void reconcile( final DirtyRegion dirtyRegion,
                         final IRegion subRegion ) {
    reconcile();
  }

  public void reconcile( final IRegion partition ) {
    reconcile();
  }


  // interface methods of IReconcilingStrategyExtension
  // ///////////////////////////////////////////////////

  public void setProgressMonitor( final IProgressMonitor monitor ) {
    //foldingStructureProvider.setProgressMonitor( monitor );
    editor.getFoldingStructureProvider().setProgressMonitor( monitor );
  }

  public void initialReconcile() {
    reconcile();
  }


  // helping methods
  // ////////////////

  private void reconcile() {
    /*Shell shell = editor.getSite().getShell();
    if( shell != null && !shell.isDisposed() ) {
      shell.getDisplay().asyncExec( new Runnable() {
        public void run() {
          if( markOccurrencesComputer != null ) {
            markOccurrencesComputer.compute();
          }
        }
      } );
      if( document != null ) {
        if( instance!=null ) {
            instance.outline(file,new OutlineHandler() {

              public void outlineResult( final List<OutlineDef> outlineDefs ) {
                if(editor.getOutlinePage()!=null){
                  editor.getOutlinePage().setInput( outlineDefs );
                }
               foldingStructureProvider.updateFoldingRegions(outlineDefs);
              }
            });
          }

        }
    }*/
    editor.synchronize();
  }
}
