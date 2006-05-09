// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.dialog;

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;

/** <p>a label provider for Haskell projects and their source folders.</p>
  * 
  * @author Administrator
  */
public class DialogLabelProvider extends LabelProvider {
  
  // interface methods of ILabelProvider
  //////////////////////////////////////
  
  public String getText( final Object element ) {
    return ResourceUtil.findResource( element ).getName();
  }
    
  public Image getImage( final Object element ) {
    Image result = null; 
    if( element instanceof IHaskellProject ) {
      result = HaskellUIImages.getImage( IImageNames.HASKELL_PROJECT );
    } else if( isSourceFolder( element ) ) {
      result = HaskellUIImages.getImage( IImageNames.SOURCE_FOLDER ); 
    } else if( element instanceof IFolder ) {
      ISharedImages si = PlatformUI.getWorkbench().getSharedImages();
      result = si.getImage( ISharedImages.IMG_OBJ_FOLDER );
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