// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.haddock.HaddockPlugin;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;

/** <p>content provider for the tree part of the viewer on the selection 
  * page.</p>
  *
  * @author Leif Frenzel
  */
class CheckboxTreeCP implements ITreeContentProvider {

  
  // interface methods of ITreeContentProvider
  ////////////////////////////////////////////
  
  public Object[] getElements( final Object inputElement ) {
    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    IProject[] projects = root.getProjects();
    return filterHsProjects( projects );
  }

  public Object[] getChildren( final Object parentElement ) {
    Object[] result = new Object[ 0 ];
    if( parentElement instanceof IProject ) {
      IProject project = ( IProject )parentElement;
      result = getProjectChildFolders( project );
    } else if( parentElement instanceof IFolder ) {
      IFolder folder = ( IFolder )parentElement;
      result = getChildFolders( folder );
    }
    return result;
  }

  private Object[] getProjectChildFolders( final IProject project ) {
    Object[] result = new Object[ 0 ];
    try {
      IContainer sourceFolder = ResourceUtil.getSourceFolder( project );
      if( sourceFolder.equals( project ) ) {
        result = getChildFolders( sourceFolder );
      } else {
        result = new Object[] { sourceFolder };
      }
    } catch( CoreException cex ) {
      String msg =   "Problem finding child folders in " 
                   + project.getName();
      HaddockPlugin.log( msg, cex );
    }
    return result;
  }

  public Object getParent( final Object element ) {
    Object result = null;
    if( element instanceof IFolder ) {
      IFolder folder = ( IFolder )element;
      IContainer sourceFolder = getSourceFolder( folder );
      if( folder.equals( sourceFolder ) ) {
        result = folder.getProject();
      } else {
        result = folder.getParent();
      }
    }
    return result;
  }

  public boolean hasChildren( final Object element ) {
    return getChildren( element ).length > 0;
  }

  public void dispose() {
    // unused
  }

  public void inputChanged( final Viewer viewer, 
                            final Object oldInput, 
                            final Object newInput ) {
    // unused
  }
  
  
  // helping methods
  //////////////////
  
  private IContainer getSourceFolder( final IFolder folder ) {
    IContainer result = null;
    try {
      result = ResourceUtil.getSourceFolder( folder.getProject() );
    } catch( CoreException cex ) {
      HaddockPlugin.log( cex.toString(), cex );
    }
    return result;
  }

  private Object[] filterHsProjects( final IProject[] projects ) {
    List list = new ArrayList();
    for( int i = 0; i < projects.length; i++ ) {
      IProject project = projects[ i ];
      try {
        if( project.hasNature( HaskellNature.NATURE_ID ) ) {
          list.add( project );
        }
      } catch( CoreException cex ) {
        HaddockPlugin.log( cex.toString(), cex );
      }
    }
    return list.toArray();
  }
  
  private Object[] getChildFolders( final IContainer container ) {
    Object[] result = new Object[ 0 ];
    try {
      IResource[] ress = container.members();
      List list = new ArrayList();
      for( int i = 0; i < ress.length; i++ ) {
        if( ress[ i ] instanceof IFolder ) {
          list.add( ress[ i ] );
        }
      }
      result = list.toArray();
    } catch( CoreException cex ) {
      String msg =   "Problem finding child folders in " 
                   + container.getName();
      HaddockPlugin.log( msg, cex );
    }
    return result;
  }
}
