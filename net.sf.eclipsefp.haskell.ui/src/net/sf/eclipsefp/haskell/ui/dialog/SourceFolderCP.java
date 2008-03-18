// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.dialog;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/** the content provider for the source folder selection dialog.
  *
  * @author Leif Frenzel
  */
class SourceFolderCP implements ITreeContentProvider {

  public Object[] getElements( final Object inputElement ) {
    return HaskellProjectManager.getAll( ( IWorkspaceRoot )inputElement );
  }

  public Object[] getChildren( final Object parentElement ) {
    List<IFolder> list = new ArrayList<IFolder>();
    if( parentElement instanceof IHaskellProject ) {
      IHaskellProject hsProject = ( IHaskellProject )parentElement;
      Set<IPath> sourcePaths = hsProject.getSourcePaths();
      for( IPath sourcePath: sourcePaths ) {
        IProject project = hsProject.getResource();
        if( !sourcePath.equals( project.getProjectRelativePath() ) ) {
          list.add( project.getFolder( sourcePath ) );
        }
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