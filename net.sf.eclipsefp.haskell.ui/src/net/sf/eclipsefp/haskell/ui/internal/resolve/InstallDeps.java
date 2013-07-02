/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.SandboxHelper;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.scion.CabalFileChangeListenerManager;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.CabalFileChangeListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;

/**
 * Install dependencies for sandboxed projects
 *
 * @author JP Moresmau
 *
 */
public class InstallDeps extends MarkerCompletion {

  public InstallDeps( ) {
    super();

  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.IMarkerResolution#getLabel()
   */
  @Override
  public String getLabel() {
    return UITexts.resolve_install_sandbox;

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.resolve.MarkerCompletion#getCompletionProposal(org.eclipse.core.resources.IMarker, org.eclipse.jface.text.IDocument)
   */
  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    return new ICompletionProposal() {

      @Override
      public Point getSelection( final IDocument arg0 ) {
        return null;
      }

      @Override
      public Image getImage() {
        return null;
      }

      @Override
      public String getDisplayString() {
       return getLabel();
      }

      @Override
      public IContextInformation getContextInformation() {
        return null;
      }

      @Override
      public String getAdditionalProposalInfo() {
        return null;
      }

      @Override
      public void apply( final IDocument arg0 ) {
        final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
        if (cabalExecutable==null){
          HaskellUIPlugin.log( UITexts.noCabalImplementationForInstall_error, IStatus.ERROR );
          return;
        }
        Job j=new Job(getLabel()) {

          @Override
          protected IStatus run( final IProgressMonitor monitor ) {
            try {
              if (ScionManager.getCabalImplDetails().isSandboxed()){
                IProject project=marker.getResource().getProject();
                BWFacade bwf=BuildWrapperPlugin.getFacade( project );
                if (bwf!=null){
                  SandboxHelper.installDeps(bwf );
                  bwf.cabalFileChanged();
                  IFile cabalF = BuildWrapperPlugin.getCabalFile( project );
                  for (CabalFileChangeListener l:CabalFileChangeListenerManager.getListeners()){
                    l.cabalFileChanged( cabalF );
                  }
                }
              }
              return Status.OK_STATUS;


            } catch (Exception ioe){
              HaskellUIPlugin.log(ioe);
              return new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);

            }
          }
        };
        j.setPriority( Job.BUILD);
        j.schedule();

      }
    };
  }
}
