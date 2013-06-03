// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;


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
      result = ResourceUtil.getSourceContainer( resource );
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
        result = ResourceUtil.getSourceRelativePath( sourceContainer, resource );
      }
    }
    return result;
  }

  static boolean isInHaskellProject( final ISelection selection ) {
    boolean result = false;
    IResource resource = getResource( selection );
    if( resource != null ) {
      IProject project = resource.getProject();
       result = ResourceUtil.hasHaskellNature(project);
    }
    return result;
  }


  // TODO this is not selection-related, move to somewhere else



  // helping methods
  //////////////////



  private static IResource getResource( final ISelection selection ) {
    IResource result = null;
    if( selection instanceof IStructuredSelection ) {
      Object obj = ( ( IStructuredSelection )selection ).getFirstElement();
      result = ResourceUtil.findResource( obj );
    }
    return result;
  }


}