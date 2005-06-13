// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.wizards;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;

import de.leiffrenzel.fp.haskell.core.project.HaskellNature;
import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;
import de.leiffrenzel.fp.haskell.ui.HaskellUIPlugin;


/** <p>helping class that knows how to get various informations out of
  * a selection.</p>
  * 
  * @author Leif Frenzel
  */
class SelectionAnalyzer {

  static IContainer getSourceContainer( final ISelection selection ) {
    IContainer result = null;
    IResource resource = getResource( selection );
    if( resource != null ) {
      result = getSourceContainer( resource );
    }
    return result;
  }

  /** returns the source folder relative folder represented by the passed
    * selection (if any) */
  static IPath getSourceRelativePath( final ISelection selection ) {
    IPath result = null;
    IContainer sourceContainer = getSourceContainer( selection );
    if( sourceContainer != null ) {
      IResource resource = getResource( selection );
      if( resource != null ) {
        result = getSourceRelativePath( sourceContainer, resource );
      }
    }
    return result;
  }

  static boolean isInHaskellProject( final ISelection selection ) {
    boolean result = false;
    IResource resource = getResource( selection );
    if( resource != null ) {
      IProject project = resource.getProject();
      try {
        result = project.hasNature( HaskellNature.NATURE_ID );
      } catch( CoreException ex ) {
        logProblem( project, ex );
      }
    }
    return result;
  }

  
  // TODO this is not selection-related, move to somewhere else
  static IPath getSourceRelativePath( final IResource resource ) {
    IPath result = null;
    IContainer sourceFolder = getSourceContainer( resource );
    if( sourceFolder != null ) {
      if( resource != null ) {
        result = getSourceRelativePath( sourceFolder, resource );
      }
    }
    return result;
  }
  
  
  // helping methods
  //////////////////
  
  private static IContainer getSourceContainer( final IResource resource ) {
    IContainer result = null;
    IProject project = resource.getProject();
    try {
      if( project.hasNature( HaskellNature.NATURE_ID ) ) {
        result = ResourceUtil.getSourceFolder( project );
      }
    } catch( CoreException ex ) {
      logProblem( project, ex );
    }
    return result;
  }
  
  private static void logProblem( final IProject project, 
                                  final CoreException ex ) {
    String msg = "Problem with project '" + project.getName() + "'.";
    HaskellUIPlugin.log( msg, ex );
  }

  private static IResource getResource( final ISelection selection ) {
    IResource result = null;
    if( selection instanceof IStructuredSelection ) {
      Object obj = ( ( IStructuredSelection )selection ).getFirstElement();
      result = ResourceUtil.findResource( obj );
    }
    return result;
  }
  
  /** returns the container this resource is in (the resource itself, if it is 
    * a container). */
  private static IContainer getContainer( final IResource resource ) {
    return ( resource instanceof IContainer ) ? ( IContainer )resource 
                                              : resource.getParent();
  }
  
  private static IPath getSourceRelativePath( final IContainer sourceContainer, 
                                              final IResource resource ) {
    IPath result = null;
    IContainer resourceContainer = getContainer( resource );
    IPath sourcePath = sourceContainer.getProjectRelativePath();
    IPath resourcePath = resourceContainer.getProjectRelativePath();
    if( sourcePath.isPrefixOf( resourcePath ) ) {
      int count = sourcePath.segmentCount();
      result = resourcePath.removeFirstSegments( count );
    }
    return result;
  }
}