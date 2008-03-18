// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.project.IImportLibrary;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;


/** <p>the content provider for the Module browser. Content is the set of
  * Haskell projects in the workspace, with specifically viewing source
  * folders, libraries, Haskell sources etc.</p>
  *
  * @author Leif Frenzel
  */
class ModuleBrowserContentProvider implements IContentProvider,
                                              ITreeContentProvider {

  private static final Object[] EMPTY = new Object[ 0 ];

  private final UIState uiState;

  ModuleBrowserContentProvider( final UIState uiState ) {
    this.uiState = uiState;
  }

  // interface methods
  ////////////////////

  public Object[] getElements( final Object inputElement ) {
    Assert.isTrue( inputElement instanceof IWorkspaceRoot );
    IWorkspaceRoot root = ( IWorkspaceRoot )inputElement;
    return HaskellProjectManager.getAll( root );
  }

  public Object[] getChildren( final Object element ) {
    Object[] result = EMPTY;
    if( element instanceof IHaskellProject ) {
      List<Object> list = new ArrayList<Object>();
      IHaskellProject hsProject = ( IHaskellProject )element;
      addSourceFolder( hsProject, list );
      if( isSourceFolder( hsProject.getResource() ) ) {
        addHaskellSources( hsProject, list );
      }
      addImportLibraries( hsProject, list );
      addProjectExecutable( hsProject, list );
      result = list.toArray();
    } else if( element instanceof IFolder ) {
      result = getFolderChildren( ( IFolder )element );
    }
    return result;
  }

  public Object getParent( final Object element ) {
    return null;
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

  private void addSourceFolder( final IHaskellProject hsProject,
                                final List<Object> list ) {
    IPath sourcePath = hsProject.getSourcePaths().iterator().next();
    if( sourcePath.segmentCount() > 0 ) {
      IProject project = hsProject.getResource();
      IFolder sourceFolder = project.getFolder( sourcePath );
      list.add( sourceFolder );
    }
  }

  private void addProjectExecutable( final IHaskellProject hsProject,
                                     final List<Object> list ) {
    IProject project = hsProject.getResource();
    try {
      IFile projectExecutable = ResourceUtil.getProjectExecutable( project );
      if( projectExecutable != null ) {
        list.add( projectExecutable );
      }
    } catch( CoreException ex ) {
      String msg =   "Problem determining project executable for "
                   + project.getName();
      HaskellUIPlugin.log( msg, ex );
    }
  }

  private void addImportLibraries( final IHaskellProject hsProject,
                                   final List<Object> list ) {
    IImportLibrary[] libs = hsProject.getImportLibraries();
    for( int i = 0; i < libs.length; i++ ) {
      if( libs[ i ].isUsed() ) {
        list.add( libs[ i ] );
      }
    }
  }

  private void addMembers( final IFolder folder, final List<IResource> list ) {
    try {
      IResource[] members = ( folder ).members();
      for( int i = 0; i < members.length; i++ ) {
        if(    members[ i ].getType() == IResource.FOLDER
            || ResourceUtil.hasHaskellExtension( members[ i ] ) ) {
          list.add( members[ i ] );
        }
      }
    } catch( CoreException ex ) {
      String msg =   "Problem reading resources in directory "
                   + folder.getName();
      HaskellUIPlugin.log( msg, ex );
    }
  }

  // this can only be the source folder or some of its subfolders
  private Object[] getFolderChildren( final IFolder folder ) {
    Object[] result = EMPTY;
    if(    !uiState.isFlatLayout()
        && Util.applyHierarchicalLayout( folder ) ) {
      try {
        IFolder subFolder = ( IFolder )folder.members()[ 0 ];
        result = getChildren( subFolder );
      } catch( CoreException cex ) {
        String msg = "Problem with children of '" + folder.getName() + "'.";
        HaskellUIPlugin.log( msg, cex );
      }
    } else {
      List<IResource> list = new ArrayList<IResource>();
      addMembers( folder, list );
      result = list.toArray();
    }
    return result;
  }

  private boolean isSourceFolder( final IProject project ) {
    return ResourceUtil.getSourceFolder( project ) == project;
  }

  private void addHaskellSources( final IHaskellProject hsProject,
                                  final List<Object> list ) {
    try {
      IResource[] ress = hsProject.getResource().members();
      for( int i = 0; i < ress.length; i++ ) {
        if(    ress[ i ]  instanceof IFile
            && ResourceUtil.hasHaskellExtension( ress[ i ] ) ) {
          list.add( ress[ i ] );
        }
      }
    } catch( CoreException cex ) {
      String msg =   "Problem with children of '"
                   + hsProject.getResource().getName()
                   + "'.";
      HaskellUIPlugin.log( msg, cex );
    }
  }
}