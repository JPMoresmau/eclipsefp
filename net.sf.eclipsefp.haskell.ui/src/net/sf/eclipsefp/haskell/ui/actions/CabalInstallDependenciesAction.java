/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.actions;

import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.JobFacade;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalImplDetails;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.backend.BackendManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.osgi.util.NLS;


/**
 * Install only the project dependencies
 * @author JP Moresmau
 *
 */
public class CabalInstallDependenciesAction extends CabalInstallAction {

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.actions.CabalInstallAction#getJobName()
   */
  @Override
  protected String getJobName() {
    return UITexts.install_dependencies_job;
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.actions.CabalInstallAction#addExtraParameters(java.util.List)
   */
  @Override
  protected void addExtraParameters( final List<String> commands ) {
    commands.add("--only-dependencies");
    /** ensure test libraries are downloaded too **/
    commands.add("--enable-tests");
  }

  @Override
  protected String getCabalDevCommand() {
    return "install-deps";
  }

  @Override
  protected String getSandboxWarningMessage() {
    return UITexts.install_sandbox_dependencies_text;
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.actions.CabalInstallAction#run(org.eclipse.jface.action.IAction)
   */
  @Override
  public void run( final IAction arg0 ) {
    CabalImplDetails cid=BackendManager.getCabalImplDetails();
    // install via sandboxhelper that will install deps of top level projects to keep consistency
    if (cid.isSandboxed() && cid.isUniqueSandbox()){
      JobFacade.installDeps( projects );
      HaskellUIPlugin.getDefault().getBackendManager().rebuildBrowser();

      return;
    }
    super.run( arg0 );
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.actions.CabalInstallAction#getAfter(org.eclipse.core.resources.IProject)
   */
  @Override
  protected Runnable getAfter(final IProject p ) {
    final Runnable rs=super.getAfter( p );
    // once we've downloaded the dependencies, we clean the project and rebuild if we're in auto build mode
    Runnable r=new Runnable(){
      @Override
      public void run() {
       if (rs!=null){
         rs.run();
       }
        final String jobName = NLS.bind(BWText.job_clean, p.getName());
        Job j=new Job(jobName) {

          @Override
          protected IStatus run( final IProgressMonitor mon ) {
            try {
              p.build( IncrementalProjectBuilder.CLEAN_BUILD , mon );
              if (ResourcesPlugin.getWorkspace().isAutoBuilding()){
                if (mon!=null){
                  mon.setTaskName( NLS.bind(BWText.job_build, p.getName()) );
                }
                p.build( IncrementalProjectBuilder.FULL_BUILD , mon );
              }
              HaskellUIPlugin.getDefault().getBackendManager().rebuildBrowser();

            } catch (CoreException ce){
              return new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), ce.getLocalizedMessage(),ce);
            }
            return Status.OK_STATUS;
          }
        };
        j.setPriority( Job.BUILD );
        j.schedule();
      }
    };
    return r;


  }
}
