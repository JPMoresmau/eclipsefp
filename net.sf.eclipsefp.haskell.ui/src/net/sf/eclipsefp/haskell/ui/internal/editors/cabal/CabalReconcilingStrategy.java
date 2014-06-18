// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import org.eclipse.jface.text.AbstractDocument;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.spelling.SpellingAnnotation;
import org.eclipse.ui.texteditor.spelling.SpellingReconcileStrategy;

/**
 * reconciler for Cabal
 *
 * @author JP Moresmau
 * @author Leif Frenzel
 */
class CabalReconcilingStrategy extends SpellingReconcileStrategy {

  private final CabalFormEditor editor;
  private final CabalFoldingStructureProvider foldingStructureProvider;

  CabalReconcilingStrategy( final CabalFormEditor editor,final ISourceViewer sourceViewer ) {
    super(sourceViewer,EditorsUI.getSpellingService());
    this.editor = editor;
    foldingStructureProvider = new CabalFoldingStructureProvider( editor );
  }


  // interface methods of IReconcilingStrategy
  ////////////////////////////////////////////

  @Override
  public void reconcile( final IRegion region ) {
    AbstractDocument document = (AbstractDocument) getDocument();
   IAnnotationModel model = getAnnotationModel();
    if (region.getOffset() == 0 && region.getLength() == document.getLength()) {
      //reconciling whole document
      super.reconcile(region);
    } else {
     //partial reconciliation
     //preserve spelling annotations first
     Iterator iter = model.getAnnotationIterator();
     Map<Annotation,Position> spellingErrors = new HashMap<>();
     while (iter.hasNext()) {
      Annotation annotation = (Annotation) iter.next();
      if (annotation instanceof SpellingAnnotation) {
       SpellingAnnotation spellingAnnotation = (SpellingAnnotation) annotation;
       Position position = model.getPosition(spellingAnnotation);

       spellingErrors.put(spellingAnnotation, position);
      }
     }

     //reconcile
     super.reconcile(region);

     //restore annotations
     model = getAnnotationModel();
     iter = spellingErrors.keySet().iterator();
     while (iter.hasNext()) {
      Annotation annotation = (Annotation) iter.next();
      model.addAnnotation(annotation, spellingErrors.get(annotation));
     }
    }
    reconcile();
  }

  @Override
  public void reconcile( final DirtyRegion dirtyRegion, final IRegion subRegion ) {
    super.reconcile( dirtyRegion, subRegion );
    reconcile();
  }

  @Override
  public void setDocument( final IDocument document ) {
    super.setDocument( document );
    foldingStructureProvider.setDocument( document );
  }


  // interface methods of IReconcilingStrategyExtension
  /////////////////////////////////////////////////////

  @Override
  public void initialReconcile() {
    reconcile();
  }


  // helping methods
  //////////////////

  private void reconcile() {

    String content = getDocument().get();
    final PackageDescription pd = PackageDescriptionLoader.load( content );
    if (editor!=null){
      Display.getDefault().asyncExec( new Runnable() {
        @Override
        public void run() {
          editor.setPackageDescription( pd );
        }
      } );
      foldingStructureProvider.updateFoldingRegions( pd );
    }
  }
}
