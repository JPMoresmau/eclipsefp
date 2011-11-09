package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model.GHCSystemLibrary;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.views.navigator.ResourceSorter;


public class HaskellResourceExtensionSorter extends ResourceSorter {

  public HaskellResourceExtensionSorter() {
    super(ResourceSorter.NAME);
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.viewers.ViewerComparator#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
   */
  @Override
  public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
    if (isCabal(e1) && isCabal(e2)) {
      return super.compare( viewer, e1, e2 );
    } else if (isCabal(e1)) {
      return -1;
    } else if (isCabal(e2)) {
      return 1;
    } else if (isSourceFolder(e1) && isSourceFolder(e2)) {
      return super.compare( viewer, e1, e2 );
    } else if (isSourceFolder(e1)) {
      return -1;
    } else if (isSourceFolder(e2)) {
      return 1;
    } else if (e1 instanceof GHCSystemLibrary && e2 instanceof GHCSystemLibrary) {
      return super.compare( viewer, e1, e2 );
    } else if (e1 instanceof GHCSystemLibrary) {
      return -1;
    } else if (e2 instanceof GHCSystemLibrary) {
      return 1;
    } else {
      return super.compare( viewer, e1, e2 );
    }
  }

  public boolean isCabal(final Object o) {
    if (o instanceof IFile) {
      IFile f = (IFile)o;
      if (FileUtil.hasCabalExtension( f )) {
        return true;
      }
    }
    return false;
  }

  public boolean isSourceFolder(final Object o) {
    if (o instanceof IFolder) {
      IFolder f = (IFolder)o;
      if (ResourceUtil.isSourceFolder( f )) {
        return true;
      }
    }
    return false;
  }

}
