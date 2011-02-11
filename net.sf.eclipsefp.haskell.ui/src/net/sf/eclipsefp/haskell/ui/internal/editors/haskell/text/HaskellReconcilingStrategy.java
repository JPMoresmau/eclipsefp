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

/** Helper class that defines the model reconciling for the Haskell
  * editor.
  *
  * @author Leif Frenzel
  */
public class HaskellReconcilingStrategy implements IReconcilingStrategy,
                                                   IReconcilingStrategyExtension {
  /** The associated Haskell editor */
  private final HaskellEditor editor;
  /** The document */
 // private IDocument document;

  public HaskellReconcilingStrategy( final HaskellEditor editor ) {
    this.editor = editor;
  //  this.document = null;
  }


  // interface methods of IReconcilingStrategy
  // //////////////////////////////////////////

  public void reconcile( final DirtyRegion dirtyRegion, final IRegion subRegion ) {
    reconcile();
  }

  public void reconcile( final IRegion partition ) {
    reconcile();
  }


  public void setDocument( final IDocument document ) {
//    this.document = document;
  }

  // interface methods of IReconcilingStrategyExtension
  // ///////////////////////////////////////////////////

  public void setProgressMonitor( final IProgressMonitor monitor ) {
    // NOP
  }

  public void initialReconcile() {
    //reconcile();
    // done in setInput
    //editor.synchronize();
  }


  // helping methods
  // ////////////////

  private void reconcile() {
    // on save we do typecheck and synchronize outline, so only use reconciler when dirty
    if (editor.isDirty()) {
      editor.updateOutline();
    }
  }
}
