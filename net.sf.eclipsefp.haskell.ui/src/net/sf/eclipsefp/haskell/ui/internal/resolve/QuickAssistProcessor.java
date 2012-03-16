package net.sf.eclipsefp.haskell.ui.internal.resolve;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.quickassist.IQuickAssistInvocationContext;
import org.eclipse.jface.text.quickassist.IQuickAssistProcessor;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.texteditor.MarkerAnnotation;

/**
 * <p>Compute quick assist based on the Marker resolution generator</p>
 *
 * @author JP Moresmau
 * @author Alejandro Serrano
 */
public class QuickAssistProcessor implements IQuickAssistProcessor {
  private final BuildMarkerResolutionGenerator generator=new BuildMarkerResolutionGenerator();

  @Override
  public boolean canAssist( final IQuickAssistInvocationContext invocationContext ) {
    return false;
  }

  @Override
  public boolean canFix( final Annotation annotation ) {
    if (annotation instanceof MarkerAnnotation){
      return generator.getResolutions( ((MarkerAnnotation)annotation ).getMarker()).length>0;
    }
    return false;
  }

  @Override
  public ICompletionProposal[] computeQuickAssistProposals(
      final IQuickAssistInvocationContext invocationContext ) {
    List<ICompletionProposal> res=new ArrayList<ICompletionProposal>();
    for (Iterator<?> it=invocationContext.getSourceViewer().getAnnotationModel().getAnnotationIterator();it.hasNext();){
      Annotation ann=(Annotation)it.next();
      if (ann instanceof MarkerAnnotation){
        Position p=invocationContext.getSourceViewer().getAnnotationModel().getPosition( ann );

        if (p.getOffset()==invocationContext.getOffset() || p.includes(invocationContext.getOffset())){
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

    return res.toArray( new ICompletionProposal[res.size()] );
  }

  @Override
  public String getErrorMessage() {
     return null;
  }

}
