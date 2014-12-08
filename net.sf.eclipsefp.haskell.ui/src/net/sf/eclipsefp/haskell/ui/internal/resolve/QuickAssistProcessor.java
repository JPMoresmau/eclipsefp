/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.quickassist.IQuickAssistProcessor;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.texteditor.MarkerAnnotation;
import org.eclipse.ui.texteditor.spelling.SpellingCorrectionProcessor;

/**
 * <p>Compute quick assist based on the Marker resolution generator</p>
 *
 * @author JP Moresmau
 * @author Alejandro Serrano
 */
public class QuickAssistProcessor implements IQuickAssistProcessor {
  private final BuildMarkerResolutionGenerator generator=new BuildMarkerResolutionGenerator();

  private final SpellingCorrectionProcessor spelling=new SpellingCorrectionProcessor();

  @Override
  public boolean canAssist( final IQuickAssistInvocationContext invocationContext ) {
    return spelling.canAssist( invocationContext );
  }

  @Override
  public boolean canFix( final Annotation annotation ) {
    boolean can=spelling.canFix( annotation );
    if (can){
      return true;
    }
    if (annotation instanceof MarkerAnnotation){
      return generator.getResolutions( ((MarkerAnnotation)annotation ).getMarker()).length>0;
    }
    return false;
  }

  @Override
  public ICompletionProposal[] computeQuickAssistProposals(
      final IQuickAssistInvocationContext invocationContext ) {
    List<ICompletionProposal> res=new ArrayList<>();
    // get line of invocation
    int line=0;
    try {
      line=invocationContext.getSourceViewer().getDocument().getLineOfOffset( invocationContext.getOffset() );
    } catch (BadLocationException ble){
      // ignore
    }
    for (Iterator<?> it=invocationContext.getSourceViewer().getAnnotationModel().getAnnotationIterator();it.hasNext();){
      Annotation ann=(Annotation)it.next();
      if (ann instanceof MarkerAnnotation){
        Position p=invocationContext.getSourceViewer().getAnnotationModel().getPosition( ann );
        // line of marker
        int line2=-1;
        try {
          line2=invocationContext.getSourceViewer().getDocument().getLineOfOffset( p.getOffset() );
        } catch (BadLocationException ble){
          // ignore
        }
        // same offset, or same line, or marker includes the offset
        if (p.getOffset()==invocationContext.getOffset() || p.includes(invocationContext.getOffset()) || line==line2){
          IMarker marker=((MarkerAnnotation)ann ).getMarker();
          IMarkerResolution[] res1=generator.getResolutions( marker);
          for (IMarkerResolution imr:res1){
            if (imr instanceof MarkerCompletion) {
              ICompletionProposal cp=((MarkerCompletion)imr).getCompletionProposal( marker, invocationContext.getSourceViewer().getDocument() );
              if (cp!=null){
                res.add(cp);
              }
            }
          }
          //res.addAll( Arrays.asList( res1 ) );
        }
      }
    }
    for (ICompletionProposal cp:spelling.computeQuickAssistProposals( invocationContext ) ){
      // do not show "no suggestions" if we have something else
      if (!cp.getClass().getName().equals( "org.eclipse.ui.internal.texteditor.spelling.NoCompletionsProposal" ) || res.isEmpty()){
        res.add( cp );
      }
    }

    return res.toArray( new ICompletionProposal[res.size()] );
  }

  @Override
  public String getErrorMessage() {
     return null;
  }

}
