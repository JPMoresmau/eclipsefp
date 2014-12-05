// Copyright (c) 2004-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.jface.text.AbstractDocument;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.spelling.SpellingAnnotation;
import org.eclipse.ui.texteditor.spelling.SpellingReconcileStrategy;

/** Helper class that defines the model reconciling for the Haskell
  * editor.
  *
  * @author Leif Frenzel
  */
public class HaskellReconcilingStrategy extends SpellingReconcileStrategy {
  /** The associated Haskell editor */
  private final HaskellEditor editor;

  private final boolean doSpell;


  public HaskellReconcilingStrategy( final HaskellEditor editor,final ISourceViewer sourceViewer,final boolean doSpell ) {
    super(sourceViewer,EditorsUI.getSpellingService());
    this.editor = editor;
    this.doSpell=doSpell;
  }


  // interface methods of IReconcilingStrategy
  // //////////////////////////////////////////

  @Override
  public void reconcile( final DirtyRegion dirtyRegion, final IRegion subRegion ) {
    if (doSpell){
      super.reconcile( dirtyRegion, subRegion );
    }
    reconcile();
  }

//  @Override
//  public void reconcile( final IRegion partition ) {
//    if (doSpell){
//      super.reconcile( partition );
//    }
//    reconcile();
//  }

  @Override
  public void reconcile(final IRegion region) {
    if (doSpell){
      AbstractDocument document = (AbstractDocument) getDocument();
      IDocumentPartitioner docPartitioner = document.getDocumentPartitioner();

      IAnnotationModel model = getAnnotationModel();
      if (region.getOffset() == 0 && region.getLength() == document.getLength()) {
      //reconciling whole document
      super.reconcile(region);
      deleteUnwantedAnnotations();
      } else {
       //partial reconciliation
       //preserve spelling annotations first
       @SuppressWarnings("unchecked")
       Iterator<Annotation> iter = model.getAnnotationIterator();
       Map<Annotation,Position> spellingErrors = new HashMap<>();
       while (iter.hasNext()) {
        Annotation annotation = iter.next();
        if (annotation instanceof SpellingAnnotation) {
         SpellingAnnotation spellingAnnotation = (SpellingAnnotation) annotation;
         Position position = model.getPosition(spellingAnnotation);
         String contentType = docPartitioner.getContentType(position.getOffset());

         if (HaskellEditor.TEXT_CONTENTTYPE.equalsIgnoreCase(contentType)) {
          spellingErrors.put(spellingAnnotation, position);
         }
        }
       }

       //reconcile
       super.reconcile(region);

       //restore annotations
       model = getAnnotationModel();
       iter = spellingErrors.keySet().iterator();
       while (iter.hasNext()) {
        Annotation annotation = iter.next();
        model.addAnnotation(annotation, spellingErrors.get(annotation));
       }
       deleteUnwantedAnnotations();
      }
    }
    reconcile();
   }

   /**
   * Deletes the spelling annotations marked for not supported content types
   */
   private void deleteUnwantedAnnotations() {
    org.eclipse.jface.text.AbstractDocument document = (org.eclipse.jface.text.AbstractDocument) getDocument();
    IDocumentPartitioner docPartitioner = document.getDocumentPartitioner();
    IAnnotationModel model = getAnnotationModel();
    Iterator<?> iter = model.getAnnotationIterator();

    while (iter.hasNext()) {
     Annotation annotation = (Annotation)iter.next();
     if (annotation instanceof SpellingAnnotation) {
      SpellingAnnotation spellingAnnotation = (SpellingAnnotation) annotation;
      Position position = model.getPosition(spellingAnnotation);
      String contentType = docPartitioner.getContentType(position.getOffset());
      if (!HaskellEditor.TEXT_CONTENTTYPE.equalsIgnoreCase(contentType)) {
       model.removeAnnotation(spellingAnnotation);
      }
     }
    }
   }

  @Override
  public void setDocument( final IDocument document ) {
    if (doSpell){
      super.setDocument( document );
    }
  }

  // interface methods of IReconcilingStrategyExtension
  // ///////////////////////////////////////////////////


  @Override
  public void initialReconcile() {
    if (doSpell){
      super.initialReconcile();
    }
  }


  // helping methods
  // ////////////////

  private void reconcile() {
    // on save we do typecheck and synchronize outline, so only use reconciler when dirty
    if (editor!=null && editor.isDirty()) {
      editor.synchronize();
    }
  }
}
