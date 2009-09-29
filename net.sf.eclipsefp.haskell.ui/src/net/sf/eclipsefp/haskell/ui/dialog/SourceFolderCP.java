// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.dialog;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/** the content provider for the source folder selection dialog.
  *
  * @author Leif Frenzel
  */
class SourceFolderCP implements ITreeContentProvider {

  public Object[] getElements( final Object inputElement ) {
    return HaskellProjectManager.getAllStandard( ( IWorkspaceRoot )inputElement );
  }

  public Object[] getChildren( final Object parentElement ) {
    List<IFolder> list = new ArrayList<IFolder>();
    /*if( parentElement instanceof IHaskellProject ) {
      IHaskellProject hsProject = ( IHaskellProject )parentElement;
      Set<IPath> sourcePaths = hsProject.getSourcePaths();
      for( IPath sourcePath: sourcePaths ) {
        IProject project = hsProject.getResource();
        if( !sourcePath.equals( project.getProjectRelativePath() ) ) {
          list.add( project.getFolder( sourcePath ) );
        }
      }
    }*/
    if (parentElement instanceof IProject){
      IProject project =(IProject)parentElement;
      IFile f=ScionInstance.getCabalFile(project  );
      try {
        PackageDescription pd=PackageDescriptionLoader.load(f);
        for (String sourcePath:pd.getStanzasBySourceDir().keySet()){
          if( sourcePath.length()>0) {
            list.add(project.getFolder( sourcePath ));
          }
        }
      } catch( CoreException ex ) {
        HaskellUIPlugin.log(  ex );
      }
    }
    return list.toArray();
  }

  public boolean hasChildren( final Object element ) {
    return element instanceof IProject;
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