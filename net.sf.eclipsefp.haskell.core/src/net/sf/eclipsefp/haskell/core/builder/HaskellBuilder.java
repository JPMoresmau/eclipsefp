// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import java.util.Map;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.IBWFacade;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.types.BuildOptions;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

/** <p>The incremental builder for Haskell projects.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellBuilder extends IncrementalProjectBuilder {

  public static final String BUILDER_ID = HaskellBuilder.class.getName();

  @Override
  protected IProject[] build( final int kind,
                              final Map args,
                              final IProgressMonitor monitor )
                                                          throws CoreException {
    //checkOutFolders( new SubProgressMonitor( monitor, 5 ) );
    checkCabalFileExists();
    performBuild( kind, monitor);//new SubProgressMonitor( monitor, 95 ) );
    //scheduleRefresh();
    return null;
  }


  private void checkCabalFileExists() throws CoreException {
    IFile cabalFile = getCabalFile();
    String id = HaskellCorePlugin.ID_PROJECT_PROBLEM_MARKER;

    if( !cabalFile.exists() ) {
      IMarker marker = getProject().createMarker( id );
      marker.setAttribute( IMarker.MESSAGE, CoreTexts.cabalBuilder_noCabal );
      marker.setAttribute( IMarker.SEVERITY, IMarker.SEVERITY_WARNING );
    } else {
      getProject().deleteMarkers( id , false, IResource.DEPTH_ZERO );
    }
  }

  private IFile getCabalFile() {
    return ScionInstance.getCabalFile( getProject() );
  }

  // helping methods
  //////////////////

  /*private void checkOutFolders( final SubProgressMonitor monitor ) {
    IWorkspaceRunnable op = new CheckOutFoldersOperation( getProject() );
    try {
      ResourcesPlugin.getWorkspace().run( op, monitor );
    } catch( CoreException cex ) {
      String msg = "Problem while checking out and bin folder existence."; //$NON-NLS-1$
      HaskellCorePlugin.log( msg, cex );
    }
  }*/

  private void performBuild( final int kind,
                             final IProgressMonitor mon ) throws CoreException {
    if( kind == IncrementalProjectBuilder.FULL_BUILD ) {
      fullBuild( mon );
    } else {
      IResourceDelta delta = getDelta( getProject() );
      if( delta == null ) {
        fullBuild( mon );
      } else {
        incrementalBuild( delta, mon );
      }
    }
  }


  /*private void scheduleRefresh() {
    Job job = new Job( CoreTexts.haskellBuilder_refreshing ) {
      @Override
      public IStatus run( final IProgressMonitor monitor ) {
        IStatus result = Status.OK_STATUS;
        try {
          getProject().refreshLocal( IResource.DEPTH_INFINITE, monitor );
        } catch( CoreException cex ) {
          String msg = "Problem during resource refresh after build."; //$NON-NLS-1$
          HaskellCorePlugin.log( msg, cex );
          result = cex.getStatus();
        }
        return result;
      }
    };
    job.schedule();
  }*/

  public void fullBuild( final IProgressMonitor mon )  {
    mon.beginTask( CoreTexts.haskellBuilder_full, 100 );
    try {
     /* IWorkspaceRunnable op = new CleanOutFoldersOperation( getProject() );
      ResourcesPlugin.getWorkspace().run( op,
                                          new SubProgressMonitor( mon, 15 ) );

      mon.subTask( CoreTexts.haskellBuilder_compiling );
      SubProgressMonitor subMon = new SubProgressMonitor( mon, 85 );
      getProject().accept( new BuildVisitor( subMon ) );*/
//      ScionInstance si = ScionPlugin.getScionInstance( getProject() );
//      if (si != null ) {
//        BuildOptions buildOptions=new BuildOptions().setOutput(true).setRecompile(false);
//        si.buildProjectForWorkspace(mon, buildOptions);
//      } else {
//        new Exception("ScionInstance == null").printStackTrace(); //$NON-NLS-1$
//      }
      IBWFacade f=BuildWrapperPlugin.getWorkspaceFacade( getProject(), mon );
      if (f!=null){
        f.build( new BuildOptions().setOutput(true).setRecompile(false) );
      } else {
        new Exception("IBWFacade == null").printStackTrace(); //$NON-NLS-1$
      }
    } finally {
      mon.done();
    }
  }

  private void incrementalBuild( final IResourceDelta delta,
                                 final IProgressMonitor mon )
                                                          throws CoreException {
    DeltaBuildVisitor v=new DeltaBuildVisitor(mon ) ;
    delta.accept(v);
    if (v.isNeedBuild()){
      fullBuild( mon );
    }
  }
}