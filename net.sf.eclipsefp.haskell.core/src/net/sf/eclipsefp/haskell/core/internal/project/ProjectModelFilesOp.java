// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.project;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalContributorManager;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.ICabalContributor;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
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

  private static final String ENC = "UTF-8"; //$NON-NLS-1$
  private static final String SETUP_HS = "Setup.hs"; //$NON-NLS-1$

  private boolean library;
  private boolean executable;


  // interface methods of IProjectCreationOperationExtraOp
  ////////////////////////////////////////////////////////

  public void run( final IProject project,
                   final IProgressMonitor mo ) throws CoreException {
    // we create nothing if no component selected, probably we're getting files from source control system or something
    if (isExecutable() || isLibrary()){
      String name = project.getName();
      createFile( project, new Path( SETUP_HS ), getSetupFileContent(), mo );

      if (isExecutable()){
        String mainPath="Main";//$NON-NLS-1$
        String src=HaskellProjectCreationOperation.getSourceDir();
        if (src!=null){
          mainPath=src+"/"+mainPath;//$NON-NLS-1$
        }

        IPath mainFile = new Path( mainPath ).addFileExtension( FileUtil.EXTENSION_HS );
        createFile( project, mainFile, getMainFileContent( ), mo  );
      }


      IPath cabalFile = new Path( name ).addFileExtension( FileUtil.EXTENSION_CABAL );
      createFile( project, cabalFile, getCabalFileContent( name ), mo  );
    }
  }


  // helping methods
  //////////////////

  private String getMainFileContent() {
   return "module Main where"+PlatformUtil.NL+PlatformUtil.NL+"main::IO()"+PlatformUtil.NL+"main = undefined"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
  }



  private String getCabalFileContent( final String name ) {

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
    PackageDescription pd=new PackageDescription( name );
    pd.getStanzas().get( 0 ).update( CabalSyntax.FIELD_VERSION, "0.1" ); //$NON-NLS-1$
    pd.getStanzas().get( 0 ).update( CabalSyntax.FIELD_CABAL_VERSION, ">= 1.2" ); //$NON-NLS-1$
    pd.getStanzas().get( 0 ).update( CabalSyntax.FIELD_BUILD_TYPE, "Simple" ); //$NON-NLS-1$
    if (isLibrary()){
      PackageDescriptionStanza pds=pd.addStanza( CabalSyntax.SECTION_LIBRARY, null );
      pds.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, FileUtil.DEFAULT_FOLDER_SRC );
      pds.update( CabalSyntax.FIELD_BUILD_DEPENDS, "base >= 4" ); //$NON-NLS-1$
      pds.update( CabalSyntax.FIELD_GHC_OPTIONS, "-Wall" ); //$NON-NLS-1$
    }

    if (isExecutable()){
      PackageDescriptionStanza pds=pd.addStanza( CabalSyntax.SECTION_EXECUTABLE, name );
      pds.update( CabalSyntax.FIELD_HS_SOURCE_DIRS, FileUtil.DEFAULT_FOLDER_SRC );
      pds.update( CabalSyntax.FIELD_MAIN_IS, "Main.hs" ); //$NON-NLS-1$
      pds.update( CabalSyntax.FIELD_BUILD_DEPENDS, "base >= 4" ); //$NON-NLS-1$
      pds.update( CabalSyntax.FIELD_GHC_OPTIONS, "-Wall" ); //$NON-NLS-1$
    }

    for (ICabalContributor c:CabalContributorManager.getContributors()){
      c.contributeOnNewProject( pd );
    }

    return pd.dump();
  }

  private void createFile( final IProject project,
                           final IPath fileName,
                           final String content,
                           final IProgressMonitor mo ) throws CoreException {
    try {
      IFile file = project.getFile( fileName );
      // file may exist if project is created from source version control
      if (!file.exists()){
        InputStream is = new ByteArrayInputStream( content.getBytes( ENC ) );
        IProgressMonitor monitor = new SubProgressMonitor( mo, 1 );
        file.create( is, true, monitor );
      }
    } catch( UnsupportedEncodingException uex ) {
      HaskellCorePlugin.log( uex );
    }
  }

  private String getSetupFileContent() {
    return "import Distribution.Simple"+PlatformUtil.NL+"main = defaultMain"+PlatformUtil.NL; //$NON-NLS-1$ //$NON-NLS-2$
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