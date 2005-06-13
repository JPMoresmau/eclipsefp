// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.dialog;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import de.leiffrenzel.fp.haskell.core.project.HaskellProjectManager;
import de.leiffrenzel.fp.haskell.core.project.IHaskellProject;

/** the content provider for the source folder selection dialog.
  * 
  * @author Leif Frenzel
  */
class SourceFolderCP implements ITreeContentProvider {

  public Object[] getElements( final Object inputElement ) {
    return HaskellProjectManager.getAll( ( IWorkspaceRoot )inputElement );
  }

  public Object[] getChildren( final Object parentElement ) {
    List list = new ArrayList();
    if( parentElement instanceof IHaskellProject ) {
      IHaskellProject hsProject = ( IHaskellProject )parentElement;
      IPath sourcePath = hsProject.getSourcePath();
      IProject project = hsProject.getResource();
      if( !sourcePath.equals( project.getProjectRelativePath() ) ) {
        list.add( project.getFolder( sourcePath ) );
      }
    }
    return list.toArray();
  }

  public boolean hasChildren( final Object element ) {
    return element instanceof IHaskellProject;
  }

  public Object getParent( final Object element ) {
    return null;
  }
  
  public void dispose() {
    // unused
  }

  public void inputChanged( final Viewer viewer, 
                            final Object oldInput, 
                            final Object newInput ) {
    // unused
  }
}