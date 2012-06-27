// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.ghci;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.BuildFlags;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

/** <p>implements a delegate for launching GHCi.</p>
  *
  * @author Leif Frenzel
  */
public class GhciLaunchOperationDelegate
       implements IInteractiveLaunchOperationDelegate {

  //private final CompilerParams compilerParams = new CompilerParams();

  // interface methods of IInteractiveLaunchOperationDelegate
  ///////////////////////////////////////////////////////////

  @Override
  public String[] createArguments( final IProject hsProject,
                                   final IFile[] selectedFiles,final String mode ) {
    List<String> cmdLine = new ArrayList<String>();

    cmdLine.add( "--interactive" ); //$NON-NLS-1$

    BWFacade bf=BuildWrapperPlugin.getFacade( hsProject );
    if (bf!=null){
      BuildFlags flags=bf.getBuildFlags( selectedFiles[0] );
      if (flags!=null){
        boolean debug="debug".equals(mode); //$NON-NLS-1$
        boolean isDir=false;
        for (String s:flags.getGhcFlags()){
          boolean include=true;
          if (debug){
            // here we exclude dir folders pointing to built object code, so that we interpret everything
            // it's better to get all flags and exclude what we don't want than adding stuff from the cabal file ourselves
            boolean removeFromDebug=(s.startsWith( "-I" ) || s.startsWith( "-i" ))  //$NON-NLS-1$ //$NON-NLS-2$
                 && s.contains( BWFacade.DIST_FOLDER );
            if (isDir){
              removeFromDebug=true;
              isDir=false;
            } else {
              isDir=s.startsWith( "-" ) && s.contains( "dir" );  //$NON-NLS-1$//$NON-NLS-2$
              removeFromDebug |= isDir;
            }
            include=!removeFromDebug;
          }
          if (include){
            cmdLine.add(s);
          }
        }
      }
    }

//    cmdLine.addAll(Util.constructLibPath( selectedFiles ));
//
//    /*if( isPrefSet( IGhcPreferenceNames.GHCI_USES_GHC_OPTIONS ) ) {
//      cmdLine.addAll( compilerParams.construct() );
//    }*/
//    for (String s:ResourceUtil.getApplicableListProperty( selectedFiles, CabalSyntax.FIELD_EXTENSIONS )){
//      if (s.length()>0){
//        cmdLine.add("-X"+s); //$NON-NLS-1$
//      }
//    }
//
//    cmdLine.addAll(ResourceUtil.getApplicableListProperty( selectedFiles, CabalSyntax.FIELD_GHC_OPTIONS ));
//
//    cmdLine.addAll(ResourceUtil.getApplicableListProperty( selectedFiles, CabalSyntax.FIELD_GHC_PROF_OPTIONS ));
//
//
//    collectImportDirs( hsProject, cmdLine ,selectedFiles );


    addAll( cmdLine, selectedFiles );
    String[] result = new String[ cmdLine.size() ];
    cmdLine.toArray( result );
    if( GhcCompilerPlugin.isTracing() ) {
      System.out.println( "Launching interactive session with arguments:" ); //$NON-NLS-1$
      HaskellCorePlugin.dump( cmdLine );
    }
    return result;
  }

  @Override
  public String getExecutable() {
    return CompilerManager.getCompilerExecutable();
  }


  // helping methods
  //////////////////

//  private void collectImportDirs( final IProject hsProject,
//                                  final List<String> cmdLine, final IFile[] selectedFiles) {
//    //if( isPrefSet( IGhcPreferenceNames.GHCI_SOURCE_FOLDERS ) ) {
//      try {
//        Set<IProject> visited = new HashSet<IProject>();
//        visited.add( hsProject );
//        collectImportDirsRec( hsProject, cmdLine, visited,selectedFiles );
//      } catch( final CoreException cex ) {
//        HaskellCorePlugin.log( cex );
//      }
//    //}
//  }
//
//  private void collectImportDirsRec(
//      final IProject hsProject,
//      final List<String> cmdLine,
//      final Set<IProject> visited,final IFile[] selectedFiles ) throws CoreException {
//    /*Set<IPath> sourcePaths = hsProject.getSourcePaths();*/
//    Collection<String> srcs=new ArrayList<String>();
//
//    srcs.addAll( ResourceUtil.getSourceFolders( selectedFiles ));
//
//    // if we reference our own library, we need to include the sources
//    // also if we don't belong to a source folder
//    if( hsProject.hasNature( HaskellNature.NATURE_ID ) ) {
//
//      IFile f=BuildWrapperPlugin.getCabalFile( hsProject );
//      PackageDescription pd=PackageDescriptionLoader.load(f);
//      if (srcs.isEmpty() || ResourceUtil.getImportPackages(selectedFiles).contains( pd.getPackageStanza().getName())){
//
//        for (PackageDescriptionStanza sts:pd.getStanzas()){
//          if (CabalSyntax.SECTION_LIBRARY.equals(sts.getType())){
//            srcs.addAll(sts.getSourceDirs());
//          }
//        }
//      }
//    }
//
//    for( String sourcePath: srcs ) {
//      IResource r=hsProject;
//      if (!sourcePath.equals( "." )){ //$NON-NLS-1$
//        r=hsProject.getFolder( sourcePath );
//      }
//      // getRawLocation gives us the real FS path even if the resource is linked
//      IPath loc = new Path( r.getLocationURI().getPath() );
//      // AbstractHaskellLaunchDelegate uses ProcessBuilder, which will do the proper escaping if needed, so no need to wrap in quotes here
//      cmdLine.add( "-i" + loc.toOSString()  ); //$NON-NLS-1$
//    }
//    IProject[] refs = hsProject.getReferencedProjects();
//    for( IProject ref: refs ) {
//      if( ref.hasNature( HaskellNature.NATURE_ID ) ) {
//        if( !visited.contains( ref ) ) {
//          collectImportDirsRec( ref, cmdLine, visited,selectedFiles );
//          visited.add( ref );
//        }
//      }
//    }
//  }

  /*private boolean isPrefSet( final String key ) {
    return Platform.getPreferencesService().getBoolean( GhcCompilerPlugin.getPluginId(), key, false, null );
  }*/

  private void addAll( final List<String> cmdLine, final IFile[] selectedFiles ) {
    for( int i = 0; i < selectedFiles.length; i++ ) {
      String path = selectedFiles[ i ].getLocation().toOSString();
      // AbstractHaskellLaunchDelegate uses ProcessBuilder, which will do the proper escaping, so no need to wrap in quotes here
      cmdLine.add( path );
    }
  }

  @Override
  public String getReloadCommand() {
    return ":r"; //$NON-NLS-1$
  }


}
