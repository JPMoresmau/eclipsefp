// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;
import de.leiffrenzel.fp.haskell.ui.dialog.DialogLabelProvider;
import de.leiffrenzel.fp.haskell.ui.util.HaskellUIImages;
import de.leiffrenzel.fp.haskell.ui.util.IImageNames;


/** <p>The label provider for the checkbox tree and list on the selection 
  * page.</p>
  *
  * @author Leif Frenzel
  */
class CheckBoxTreeAndListLP extends DialogLabelProvider {

  // interface methods of LabelProvider
  /////////////////////////////////////

  public Image getImage( final Object element ) {
    Image result = super.getImage( element );
    if( element instanceof IProject ) {
      ISharedImages si = PlatformUI.getWorkbench().getSharedImages();
      result = si.getImage( IDE.SharedImages.IMG_OBJ_PROJECT );
    } else if( isSourceFile( element, ResourceUtil.EXTENSION_HS ) ) {
      result = HaskellUIImages.getImage( IImageNames.SOURCE_FILE );
    } else if( isSourceFile( element, ResourceUtil.EXTENSION_LHS ) ) {
      result = HaskellUIImages.getImage( IImageNames.LITERATE_SOURCE_FILE );
    }
    return result;
  }

  private boolean isSourceFile( final Object element, final String extension ) {
    boolean result = false;
    if( element instanceof IFile ) {
      IFile file = ( IFile )element;
      String ext = file.getFileExtension();
      result = ext.equals( extension );
    }
    return result;
  }
}