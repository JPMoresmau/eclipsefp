// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.dialog;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
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

  @Override
  public Object[] getElements( final Object inputElement ) {
    return ResourceUtil.getHaskellProjects( ( IWorkspaceRoot )inputElement );
  }

  @Override
  public Object[] getChildren( final Object parentElement ) {
    List<IFolder> list = new ArrayList<>();
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
      IFile f=BuildWrapperPlugin.getCabalFile(project  );
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

  @Override
  public boolean hasChildren( final Object element ) {
    return element instanceof IProject;
  }

  @Override
  public Object getParent( final Object element ) {
    return null;
  }

  @Override
  public void dispose() {
    // unused
  }

  @Override
  public void inputChanged( final Viewer viewer,
                            final Object oldInput,
                            final Object newInput ) {
    // unused
  }
}