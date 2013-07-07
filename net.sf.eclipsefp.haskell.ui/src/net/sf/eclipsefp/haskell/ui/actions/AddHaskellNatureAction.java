/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.actions;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

/**
 * add haskell nature to project
 *
 * @author JP Moresmau
 *
 */
public class AddHaskellNatureAction implements IObjectActionDelegate {
  private final Set<IProject> projects=new LinkedHashSet<IProject>();

  @Override
  public void run( final IAction action ) {
    IProgressMonitor mon = new NullProgressMonitor();
    IWorkspaceRunnable operation = new IWorkspaceRunnable() {

      @Override
      public void run( final IProgressMonitor monitor ) throws CoreException {

        monitor.subTask( CoreTexts.projectCreationOperation_natures );
        for (IProject project:projects){
          addNature( monitor, project,getNature() );
        }

      }
    };
    try {
      ResourcesPlugin.getWorkspace().run( operation, mon );
    } catch( CoreException cex ) {
      HaskellCorePlugin.log( CoreTexts.projectCreationOperation_error, cex );
    } finally {
      mon.done();
    }

  }

  protected String getNature(){
    return HaskellNature.NATURE_ID ;
  }

  /**
   * add the requested nature to the project
   * @param mon
   * @param project
   * @param nature
   * @throws CoreException
   */
  private void addNature(final IProgressMonitor mon,final IProject project,final String nature) throws CoreException{
    IProjectDescription desc = project.getDescription();
    String[] nids=desc.getNatureIds();
    List<String> ls=Arrays.asList( nids );
    if (!ls.contains( nature )){
      // asList gives unmodified
      ls=new ArrayList<String>(ls);
      ls.add( nature );
      desc.setNatureIds( ls.toArray( new String[ls.size()] ) );
      project.setDescription( desc, new SubProgressMonitor( mon, 1 ) );
    }
  }

  @Override
  public void selectionChanged( final IAction action, final ISelection selection ) {
    projects.clear();
    projects.addAll( getProjects( selection ) );

  }

  /**
   * get projects that don't have out nature
   * @param arg1
   * @return
   */
  public Collection<IProject> getProjects(final ISelection arg1 ){
    Set<IProject> projects=new LinkedHashSet<IProject>();
    if (arg1 instanceof IStructuredSelection){
      for (Iterator<?> it=((IStructuredSelection)arg1).iterator();it.hasNext();){
        IResource res = ResourceUtil.findResource( it.next() );
          if( res != null && res.getProject()!=null  && !hasNature(res.getProject())) {
            projects.add( res.getProject() );
          }
      }
    }
    return projects;
  }

  public boolean hasNature(final IProject p){
    try {
      return p!=null && p.isAccessible() && p.hasNature(getNature());
    } catch (CoreException ce){
      HaskellUIPlugin.log( ce );
    }
    return false;
  }

  @Override
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart ) {
    // NOOP

  }

}
