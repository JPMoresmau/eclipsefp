// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.dialog;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/** <p>a label provider for Haskell projects and their source folders.</p>
  *
  * @author Administrator
  */
public class DialogLabelProvider extends LabelProvider {

  // interface methods of ILabelProvider
  //////////////////////////////////////

  @Override
  public String getText( final Object element ) {
    return ResourceUtil.findResource( element ).getName();
  }

  @Override
  public Image getImage( final Object element ) {
    Image result = null;
    if( element instanceof IProject  && ResourceUtil.hasHaskellNature( (IProject )element )) {
      result = HaskellUIImages.getImage( IImageNames.HASKELL_PROJECT );
    } else if( isSourceFolder( element ) ) {
      result = HaskellUIImages.getImage( IImageNames.SOURCE_FOLDER );
    } else if( element instanceof IFolder ) {
      ISharedImages si = PlatformUI.getWorkbench().getSharedImages();
      result = si.getImage( ISharedImages.IMG_OBJ_FOLDER );
    } else if (element instanceof IProject){
      result = HaskellUIImages.getImage( IImageNames.HASKELL_PROJECT );
    }
    return result;
  }

  // helping methods
  //////////////////

  private boolean isSourceFolder( final Object object ) {
    return    object instanceof IFolder
           && ResourceUtil.isSourceFolder( ( IFolder )object );
  }
}