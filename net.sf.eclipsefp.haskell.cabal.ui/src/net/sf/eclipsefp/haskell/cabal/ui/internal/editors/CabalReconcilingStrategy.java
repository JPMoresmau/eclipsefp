// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors;

import net.sf.eclipsefp.haskell.cabal.core.model.PackageDescription;
import net.sf.eclipsefp.haskell.cabal.core.model.PackageDescriptionLoader;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.eclipse.jface.text.reconciler.IReconcilingStrategyExtension;
import org.eclipse.swt.widgets.Display;

class CabalReconcilingStrategy implements IReconcilingStrategy, 
                                                 IReconcilingStrategyExtension { 

  private final CabalEditor editor;
  private IDocument document;
  private CabalFoldingStructureProvider foldingStructureProvider;
  
  CabalReconcilingStrategy( final CabalEditor editor ) {
    this.editor = editor;
    foldingStructureProvider = new CabalFoldingStructureProvider( editor );
  }

  
  // interface methods of IReconcilingStrategy
  ////////////////////////////////////////////
  
  public void reconcile( final IRegion partition ) {
    reconcile();
  }

  public void reconcile( final DirtyRegion dirtyRegion, final IRegion subRegion ) {
    reconcile();
  }

  public void setDocument( final IDocument document ) {
    this.document = document;
    foldingStructureProvider.setDocument( document );
  }

  
  // interface methods of IReconcilingStrategyExtension
  /////////////////////////////////////////////////////
  
  public void initialReconcile() {
    reconcile();
  }

  public void setProgressMonitor( final IProgressMonitor monitor ) {
    // unused
  }
  
  
  // helping methods
  //////////////////
  
  private void reconcile() {
    String content = document.get();
    final PackageDescription pd = PackageDescriptionLoader.load( content );
    Display.getDefault().asyncExec( new Runnable() {
      public void run() {
        editor.setPackageDescription( pd );
      }
    } );
    foldingStructureProvider.updateFoldingRegions( pd );
  }
}
