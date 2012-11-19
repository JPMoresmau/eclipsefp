// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.project;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalContributorManager;
import net.sf.eclipsefp.haskell.core.cabalmodel.ICabalContributor;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.preferences.TemplateVariables;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;



public class ProjectModelFilesOp implements IProjectCreationOperationExtraOp {

  private static final String SETUP_HS = "Setup.hs"; //$NON-NLS-1$

  private boolean library;
  private boolean executable;


  // interface methods of IProjectCreationOperationExtraOp
  ////////////////////////////////////////////////////////

  @Override
  public void run( final IProject project,
                   final IProgressMonitor mo ) throws CoreException {
    // we create nothing if no component selected, probably we're getting files from source control system or something
    if (isExecutable() || isLibrary()){
      String name = project.getName();
      String src=HaskellProjectCreationOperation.getSourceDir();
      Map<String,String> vars=new HashMap<String, String>();
      /**
       * put vars
       */
      vars.put( TemplateVariables.PROJECT_NAME, name );
      vars.put( TemplateVariables.SRC, src );
      vars.put( TemplateVariables.USER_NAME, PlatformUtil.getCurrentUser() );

      // we create a normal setup, but it could be a literate file
      IFile litSetup = project.getFile( "Setup.lhs" ); //$NON-NLS-1$
      // file may exist if project is created from source version control
      if (!litSetup.exists()){
        createFile( project, new Path( SETUP_HS ), getSetupFileContent(vars), mo );
      }


      if (isExecutable()){
        String mainPath="Main";//$NON-NLS-1$

        if (src!=null){
          mainPath=src+"/"+mainPath;//$NON-NLS-1$
        }

        IPath mainFile = new Path( mainPath ).addFileExtension( FileUtil.EXTENSION_HS );
        createFile( project, mainFile, getMainFileContent(vars ), mo  );
      }


      IPath cabalFile = new Path( name ).addFileExtension( FileUtil.EXTENSION_CABAL );
      createFile( project, cabalFile, getCabalFileContent( vars ), mo  );
    }
  }


  // helping methods
  //////////////////

  protected String getMainFileContent(final Map<String,String> vars ) {
    /** get module header **/
    vars.put( TemplateVariables.MODULE_NAME, "Main" );//$NON-NLS-1$
    String mod=HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_MODULE, vars );
    /** get content **/
    vars.put( TemplateVariables.MODULE, mod );
    return HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_MAIN, vars );
   //return "module Main where"+PlatformUtil.NL+PlatformUtil.NL+"main::IO()"+PlatformUtil.NL+"main = undefined"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }



  private String getCabalFileContent( final Map<String,String> vars ) {
    return getCabalFile( vars).dump();
  }

  protected PackageDescription getCabalFile(final Map<String,String> vars){

    /*String s=CabalSyntax.FIELD_NAME.getCabalName()+":           " + name + NL //$NON-NLS-1$
           + CabalSyntax.FIELD_VERSION.getCabalName()+":        0.1 "+ NL + NL //$NON-NLS-1$
           ;
    if (isLibrary()){
      s+= CabalSyntax.SECTION_LIBRARY.getCabalName() + NL
        + "  "+ CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName()+": src" + NL + NL//$NON-NLS-1$ //$NON-NLS-2$
      ;
    }

    if (isExecutable()){
        s+= CabalSyntax.SECTION_EXECUTABLE.getCabalName()+" " + name + NL //$NON-NLS-1$
           + "  "+ CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName()+": src" + NL //$NON-NLS-1$ //$NON-NLS-2$
           + "  "+ CabalSyntax.FIELD_MAIN_IS.getCabalName()+": Main.hs"+ NL + NL; //$NON-NLS-1$ //$NON-NLS-2$
    }


    return s;*/
    /*PackageDescription pd=new PackageDescription( name );
    pd.getStanzas().get( 0 ).update( CabalSyntax.FIELD_VERSION, "0.1" ); //$NON-NLS-1$
    pd.getStanzas().get( 0 ).update( CabalSyntax.FIELD_CABAL_VERSION, ">= 1.2" ); //$NON-NLS-1$
    pd.getStanzas().get( 0 ).update( CabalSyntax.FIELD_BUILD_TYPE, "Simple" ); //$NON-NLS-1$
    String userName=PlatformUtil.getCurrentUser();
    if (userName!=null){
      pd.getStanzas().get( 0 ).update( CabalSyntax.FIELD_AUTHOR, userName );
    }

    if (isLibrary()){
      PackageDescriptionStanza pds=pd.addStanza( CabalSyntax.SECTION_LIBRARY, null );
      pds.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, src );
      pds.update( CabalSyntax.FIELD_BUILD_DEPENDS, "base >= 4" ); //$NON-NLS-1$
      pds.update( CabalSyntax.FIELD_GHC_OPTIONS, "-Wall" ); //$NON-NLS-1$
    }

    if (isExecutable()){
      PackageDescriptionStanza pds=pd.addStanza( CabalSyntax.SECTION_EXECUTABLE, name );
      pds.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, src );
      pds.update( CabalSyntax.FIELD_MAIN_IS, "Main.hs" ); //$NON-NLS-1$
      pds.update( CabalSyntax.FIELD_BUILD_DEPENDS, "base >= 4" ); //$NON-NLS-1$
      pds.update( CabalSyntax.FIELD_GHC_OPTIONS, "-Wall" ); //$NON-NLS-1$
    }*/

    String library=""; //$NON-NLS-1$

    if (isLibrary()){
      library=HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_CABAL_LIBRARY, vars );
    }
    vars.put( TemplateVariables.LIBRARY, library );

    String exe="";//$NON-NLS-1$
    if (isExecutable()){
      exe=HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_CABAL_EXE, vars );
    }
    vars.put( TemplateVariables.EXECUTABLE, exe );

    String content=HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_CABAL, vars);
    PackageDescription pd=PackageDescriptionLoader.load( content );

    for (ICabalContributor c:CabalContributorManager.getContributors()){
      c.contributeOnNewProject( pd );
    }

    return pd;
  }

  private void createFile( final IProject project,
                           final IPath fileName,
                           final String content,
                           final IProgressMonitor mo ) throws CoreException {
    try {
      IFile file = project.getFile( fileName );
      // file may exist if project is created from source version control
      if (!file.exists()){
        InputStream is = new ByteArrayInputStream( content.getBytes( FileUtil.UTF8 ) );
        IProgressMonitor monitor = new SubProgressMonitor( mo, 1 );
        file.create( is, true, monitor );
        file.setCharset( FileUtil.UTF8 ,mo);
      }
    } catch( UnsupportedEncodingException uex ) {
      HaskellCorePlugin.log( uex );
    }
  }


  private String getSetupFileContent(final Map<String,String> vars) {
    //return "import Distribution.Simple"+PlatformUtil.NL+"main = defaultMain"+PlatformUtil.NL; //$NON-NLS-1$ //$NON-NLS-2$
    return HaskellCorePlugin.populateTemplate( ICorePreferenceNames.TEMPLATE_CABAL_SETUP, vars );
  }


  public boolean isLibrary() {
    return library;
  }


  public void setLibrary( final boolean library ) {
    this.library = library;
  }


  public boolean isExecutable() {
    return executable;
  }


  public void setExecutable( final boolean executable ) {
    this.executable = executable;
  }

}