// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.ghci;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.ui.internal.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import net.sf.eclipsefp.haskell.ghccompiler.core.Util;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;

/** <p>implements a delegate for launching GHCi.</p>
  *
  * @author Leif Frenzel
  */
public class GhciLaunchOperationDelegate
       implements IInteractiveLaunchOperationDelegate {

  //private final CompilerParams compilerParams = new CompilerParams();

  // interface methods of IInteractiveLaunchOperationDelegate
  ///////////////////////////////////////////////////////////

  public String[] createArguments( final IHaskellProject hsProject,
                                   final IFile[] selectedFiles ) {
    List<String> cmdLine = new ArrayList<String>();

    cmdLine.add( "--interactive" ); //$NON-NLS-1$

    String libPath = Util.constructLibPath( hsProject.getResource(),selectedFiles );
    if( !"".equals( libPath ) ) { //$NON-NLS-1$
      cmdLine.add(libPath);
    }
    /*if( isPrefSet( IGhcPreferenceNames.GHCI_USES_GHC_OPTIONS ) ) {
      cmdLine.addAll( compilerParams.construct() );
    }*/
    for (String s:ResourceUtil.getApplicableListProperty( selectedFiles, CabalSyntax.FIELD_EXTENSIONS )){
      if (s.length()>0){
        cmdLine.add("-X"+s); //$NON-NLS-1$
      }
    }

    cmdLine.addAll(ResourceUtil.getApplicableListProperty( selectedFiles, CabalSyntax.FIELD_GHC_OPTIONS ));

    cmdLine.addAll(ResourceUtil.getApplicableListProperty( selectedFiles, CabalSyntax.FIELD_GHC_PROF_OPTIONS ));


    collectImportDirs( hsProject, cmdLine ,selectedFiles );
    addAll( cmdLine, selectedFiles );
    String[] result = new String[ cmdLine.size() ];
    cmdLine.toArray( result );
    if( GhcCompilerPlugin.isTracing() ) {
      System.out.println( "Launching interactive session with arguments:" ); //$NON-NLS-1$
      HaskellCorePlugin.dump( cmdLine );
    }
    return result;
  }

  public String getExecutable() {
    return Util.getCompilerExecutable();
  }


  // helping methods
  //////////////////

  private void collectImportDirs( final IHaskellProject hsProject,
                                  final List<String> cmdLine, final IFile[] selectedFiles) {
    if( isPrefSet( IGhcPreferenceNames.GHCI_SOURCE_FOLDERS ) ) {
      try {
        Set<IHaskellProject> visited = new HashSet<IHaskellProject>();
        visited.add( hsProject );
        collectImportDirsRec( hsProject, cmdLine, visited,selectedFiles );
      } catch( final CoreException cex ) {
        HaskellCorePlugin.log( cex );
      }
    }
  }

  private void collectImportDirsRec(
      final IHaskellProject hsProject,
      final List<String> cmdLine,
      final Set<IHaskellProject> visited,final IFile[] selectedFiles ) throws CoreException {
    /*Set<IPath> sourcePaths = hsProject.getSourcePaths();*/
    for( String sourcePath: ResourceUtil.getSourceDirs( selectedFiles ) ) {
      IFolder folder = hsProject.getResource().getFolder( sourcePath );
      // getRawLocation gives us the real FS path even if the resource is linked
      IPath loc = new Path( folder.getLocationURI().getPath() );
      cmdLine.add( "-i\"" + loc.toOSString() + "\"" ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    IProject[] refs = hsProject.getResource().getReferencedProjects();
    for( IProject ref: refs ) {
      if( ref.hasNature( HaskellNature.NATURE_ID ) ) {
        IHaskellProject refHsProject = HaskellProjectManager.get( ref );
        if( !visited.contains( refHsProject ) ) {
          collectImportDirsRec( refHsProject, cmdLine, visited,selectedFiles );
          visited.add( refHsProject );
        }
      }
    }
  }

  private boolean isPrefSet( final String key ) {
    return Platform.getPreferencesService().getBoolean( GhcCompilerPlugin.getPluginId(), key, false, null );
  }

  private void addAll( final List<String> cmdLine, final IFile[] selectedFiles ) {
    for( int i = 0; i < selectedFiles.length; i++ ) {
      String path = selectedFiles[ i ].getLocation().toOSString();
      cmdLine.add( "\"" + path + "\"" ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }
}
