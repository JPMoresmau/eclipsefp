/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.SingleJobQueue;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.TextPresentation;
import org.eclipse.jface.text.presentation.PresentationReconciler;

/**
 * this presentation reconciler uses a Job since syntax coloring via buildwrapper can take some time
 * @author JP Moresmau
 *
 */
public class HaskellPresentationReconciler extends PresentationReconciler{
   private  ITextViewer viewer;
   private final SingleJobQueue queue=new SingleJobQueue();

  /* (non-Javadoc)
   * @see org.eclipse.jface.text.presentation.PresentationReconciler#createPresentation(org.eclipse.jface.text.IRegion, org.eclipse.jface.text.IDocument)
   */
  @Override
  protected TextPresentation createPresentation( final IRegion damage,
      final IDocument document ) {
    queue.addJob( new SyntaxColoringJob(damage,document));
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.text.presentation.PresentationReconciler#install(org.eclipse.jface.text.ITextViewer)
   */
  @Override
  public void install( final ITextViewer viewer ) {
     super.install( viewer );
     this.viewer=viewer;
  }

  /**
   * the job doing the syntax coloring
   * @author JP Moresmau
   *
   */
  private class SyntaxColoringJob extends Job {
    final IRegion damage;
    final IDocument document;

    public SyntaxColoringJob( final IRegion damage, final IDocument document ) {
      super( UITexts.job_syntax_coloring );
      this.damage = damage;
      this.document = document;
    }

    @Override
    protected IStatus run( final IProgressMonitor arg0 ) {
      final TextPresentation p=HaskellPresentationReconciler.super.createPresentation( damage, document );
      if (p!=null && viewer!=null && viewer.getTextWidget()!=null && !viewer.getTextWidget().isDisposed()){
        viewer.getTextWidget().getDisplay().syncExec( new Runnable(){
          /* (non-Javadoc)
           * @see java.lang.Runnable#run()
           */
          public void run() {
            viewer.changeTextPresentation(p, false);
          }
        });

      }
      return Status.OK_STATUS;
    }
  }
}