// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.JobFacade;
import net.sf.eclipsefp.haskell.buildwrapper.SandboxHelper;
import net.sf.eclipsefp.haskell.buildwrapper.WorkspaceFacade;
import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

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
    // disable output if asked for
    boolean output=true;
    if (args!=null ){
      Object o=args.get("output") ;//$NON-NLS-1$
      if (o instanceof String){
        String s=(String)o;
        if ("false".equalsIgnoreCase( s )){ //$NON-NLS-1$
          output=false;
        }
      }
    }
    performBuild( kind,output, monitor);//new SubProgressMonitor( monitor, 95 ) );
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
    return BuildWrapperPlugin.getCabalFile( getProject() );
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

  private void performBuild( final int kind,final boolean output,
                             final IProgressMonitor mon ) throws CoreException {
    if( kind == FULL_BUILD ) {
      fullBuild(true,output, mon );
    } else if( kind == CLEAN_BUILD ) {
      clean(mon);
    } else {
      IResourceDelta delta = getDelta( getProject() );
      if( delta == null ) {
        fullBuild(true,output, mon );
      } else {
        incrementalBuild( delta,output, mon );
      }
    }
  }


  @Override
  public ISchedulingRule getRule( final int kind, final Map<String, String> args ) {
    // prevent other project operations, but operations elsewhere in the workspace are fine
    return getProject();
  }

  @Override
  protected void clean( final IProgressMonitor mon) throws CoreException {
    // need a workspace operation here
    WorkspaceFacade f=BuildWrapperPlugin.getWorkspaceFacade( getProject(),mon);
    if (f!=null){
      // clean
      f.clean( mon );
    }
  }

  public void fullBuild(final boolean synchronize,final boolean output, final IProgressMonitor mon )  {
    mon.beginTask( CoreTexts.haskellBuilder_full, 100 );
    try {
      final JobFacade f=BuildWrapperPlugin.getJobFacade( getProject());
      if (f!=null){

        BuildWrapperPlugin.deleteProblems( getProject() );
        cleanNonHaskellSources();
        if (mon.isCanceled()){
          return;
        }
        final BuildOptions bo=new BuildOptions().setOutput(output).setRecompile(false);
        Runnable r=new Runnable(){
          @Override
          public void run() {
            if (synchronize){
              addProjectDependencies( f.getRealFacade(), getProject() );
              f.synchronizeAndBuild( false, bo, mon );
            } else {
              f.build( bo,mon );
            }
          }
        };
        f.getRealFacade().waitForThread( r, mon );
      } else {
        new Exception("JobFacade == null").printStackTrace(); //$NON-NLS-1$
      }
    } finally {
      mon.done();
    }
  }

  public static void addProjectDependencies(final BWFacade f,final IProject prj){
    if (f!=null && f.getCabalImplDetails().isSandboxed()){
      IWorkspaceRoot root=ResourcesPlugin.getWorkspace().getRoot();
      try {
        PackageDescription pd=PackageDescriptionLoader.load( BuildWrapperPlugin.getCabalFile( prj ) );
        Set<String> pkgs=new HashSet<String>();
        for (PackageDescriptionStanza pds:pd.getStanzas()){
          pkgs.addAll( pds.getDependentPackages() );
        }
        Set<IProject> deps=new HashSet<IProject>();
        for (String pkg:pkgs){
            IProject p=root.getProject( pkg );
            // avoid reference to itself
            if (ResourceUtil.hasHaskellNature( p ) && p!=prj){
              deps.add(p);
            }
        }

        if (deps.size()>0){

            IProjectDescription desc=prj.getDescription();
            IProject[] oldDeps=desc.getReferencedProjects();
            deps.addAll( Arrays.asList( oldDeps ) );
            IProject[] newDeps=deps.toArray( new IProject[deps.size()] );
            desc.setReferencedProjects( newDeps );
            prj.setDescription( desc, new NullProgressMonitor() );
            /**
             * we need to make sure dependencencies are added before the synchronize
             */
            f.cabalFileChanged();
            SandboxHelper.installDeps(f);
        }

      } catch(CoreException ce){
        HaskellCorePlugin.log( ce );
      }
    }
  }

  private void cleanNonHaskellSources(){
    IFile cabal=BuildWrapperPlugin.getCabalFile( getProject() );
    try {
      PackageDescription pd=PackageDescriptionLoader.load(cabal);
      Collection<String> nhfs=pd.getAllNonHaskellFiles();
      for (String s:nhfs){
        if (s!=null && s.length()>0){
          IResource r=getProject().findMember( s );
          if (r!=null){
            BuildWrapperPlugin.deleteProblems( r );
          }
        }
      }
    } catch (CoreException ce){
      HaskellCorePlugin.log( ce );
    }
  }

  private void incrementalBuild( final IResourceDelta delta,
                                 final boolean output,
                                 final IProgressMonitor mon )
                                                          throws CoreException {
    DeltaBuildVisitor v=new DeltaBuildVisitor(mon ) ;
    delta.accept(v);
    if (v.isNeedBuild()){
      // we don't know if a save was done via the editor, which does the synchronize on the exact file for us
      // or via a refactoring, which doesn't
      // v.isNeedSynchronize()
      fullBuild(true,output, mon );
    }
  }
}