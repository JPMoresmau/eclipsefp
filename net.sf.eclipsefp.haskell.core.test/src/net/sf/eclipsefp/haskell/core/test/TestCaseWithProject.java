// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.test;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.core.compiler.HsImplementation;
import net.sf.eclipsefp.haskell.core.compiler.HsImplementationPersister;
import net.sf.eclipsefp.haskell.core.compiler.HsImplementationType;
import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectModelFilesOp;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation;
import net.sf.eclipsefp.haskell.ui.wizards.ModuleCreationOperation;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.osgi.service.prefs.BackingStoreException;

/** <p>convenience super class for test cases that need a Haskell project
  * in the workspace.</p>
  *
  * @author Leif Frenzel
  */
public class TestCaseWithProject extends TestCaseWithPreferences {

  protected static final String PROJECT_NAME = "p1";
  protected IProject project;

  public TestCaseWithProject() {
    addQualifier( HaskellCorePlugin.getPluginId() );
  }



  public TestCaseWithProject( final String name ) {
    super( name );
    addQualifier( HaskellCorePlugin.getPluginId() );
  }



  public static void waitForAutoBuild() throws CoreException {
    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    workspace.build( IncrementalProjectBuilder.CLEAN_BUILD, null );
    // System.out.print( "  Waiting for autobuild to complete ..." ); //$NON-NLS-1$
    IJobManager jobMan = Job.getJobManager();
    boolean retry = true;
    while( retry ) {
      try {
        jobMan.join( ResourcesPlugin.FAMILY_AUTO_REFRESH, null );
        jobMan.join( ResourcesPlugin.FAMILY_AUTO_BUILD, null );
        jobMan.join( ResourcesPlugin.FAMILY_MANUAL_BUILD, null );
        retry = false;
      } catch (Exception exc) {
        // ignore and retry
      }
    }
    // System.out.print( " OK.\n" ); //$NON-NLS-1$
  }

  protected void checkProblems() throws CoreException{
    IMarker[] ms=project.findMarkers( IMarker.PROBLEM, true, IResource.DEPTH_INFINITE );
    StringBuilder sb=new StringBuilder();
    if (ms.length>0){
      for (IMarker m:ms){
        String s=m.getAttribute( IMarker.MESSAGE, "" );
        sb.append( s+PlatformUtil.NL );
        System.err.println(s);
      }
    }
    assertEquals(sb.toString(),0,ms.length);
  }

  public IFile addFile(final String module,final String source) {

    try {
      IFile cabal=BuildWrapperPlugin.getCabalFile( project );
      //waitForScion(cabal);
      PackageDescription pd=PackageDescriptionLoader.load(cabal);
      Map<String,List<PackageDescriptionStanza>> m=pd.getStanzasBySourceDir();
      Map.Entry<String,List<PackageDescriptionStanza>> e=m.entrySet().iterator().next();
      IFile f=project.getFile( e.getKey()+"/"+module+"."+FileUtil.EXTENSION_HS );
      if (!f.exists()){
        f.create( new ByteArrayInputStream( source.getBytes( "ASCII" ) ), true,  null);
      } else {
        f.setContents( new ByteArrayInputStream( source.getBytes( "ASCII" ) ), true, false, null );
      }
      if (!module.equals( "Main" )){
        ModuleCreationInfo mci=new ModuleCreationInfo( f );
        mci.setIncluded(Collections.singleton( e.getValue().get( 0 )));
        mci.setExposed( Collections.<PackageDescriptionStanza>emptySet() );
        ModuleCreationOperation mco=new ModuleCreationOperation( mci );
        mco.setGeneratedFile( f );
        mco.run( null );
      }
     // waitForScion(f);
     // waitForAutoBuild();
     // ScionPlugin.getScionInstance( f ).loadFile( f );
     // waitForScion(f);
      checkProblems();

      return f;
    } catch (Exception ce){
      ce.printStackTrace();
      fail( ce.getLocalizedMessage() );
    }
    return null;
  }


//  public static void waitForScion(final IResource r) throws CoreException {
//    IJobManager jobMan = Job.getJobManager();
//    Object family=ScionPlugin.getScionInstance( r );
//    if (family!=null){
//      boolean retry = true;
//      while( retry ) {
//        try {
//          jobMan.join( family, null );
//
//          retry = false;
//        } catch (Exception exc) {
//          // ignore and retry
//        }
//      }
//    }
//  }

  // interface methods of TestCase
  ////////////////////////////////

  @Override
  protected void setUp() throws Exception {
    super.setUp();

    String ghc = System.getProperty( "GHC_TEST" );
    if (ghc==null){
      fail( "no GHC_TEST system property found (must point to GHC bin folder)" );
    }
    if (!new File(ghc).exists()){
      fail( "GHC_TEST system property does not exist (must point to GHC bin folder)" );
    }
   // "D:\\dev\\haskell\\HaskellPlatform\\2010.1.0.0\\bin"
    HsImplementation impl=new HsImplementation();
    impl.setName("GHC Test");
    impl.setType( HsImplementationType.GHC );
    impl.setBinDir(ghc );
   // impl.setLibDir( "D:\\dev\\haskell\\HaskellPlatform\\2010.1.0.0\\lib" );
    impl.setVersion( "Test" );
    IEclipsePreferences node = HaskellCorePlugin.instanceScopedPreferences();

    node.put( ICorePreferenceNames.HS_IMPLEMENTATIONS, HsImplementationPersister.toXML( Collections.<IHsImplementation>singletonList( impl ) ) );
    node.put( ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION, impl.getName() );

    try {
      node.flush();
    } catch( BackingStoreException ex ) {
      fail(ex.getLocalizedMessage());
    }

    ProjectCreationOperation op = new HaskellProjectCreationOperation();
    ProjectModelFilesOp modelFiles=new ProjectModelFilesOp() ;
    modelFiles.setExecutable( true );
    op.setExtraOperation( modelFiles);
    op.setProjectName( PROJECT_NAME );
    op.run( null );



    IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
    project = wsRoot.getProject( PROJECT_NAME );

    //IFile cabal=BuildWrapperPlugin.getCabalFile( project );
    //waitForScion(cabal);

    checkProblems();
  }

  @Override
  protected void tearDown() throws Exception {
    try {
      //waitForScion(project);
      //ScionInstance si=ScionPlugin.getScionInstance( project );
      project.close( new NullProgressMonitor() );
      //waitForScion(project);
      /*synchronized(this){
        while (!si.isStopped()){
          wait(100);
        }
      }*/
      project.delete( true, true, null );
    } finally {
      super.tearDown();
    }
  }
}
