// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.*;

import net.sf.eclipsefp.haskell.core.project.IImportLibrary;

/** <p>the label provider for ImportLibraries on the viewer.</p>
  * 
  * @author Leif Frenzel
  */
class ImportLibrariesLabelProvider extends LabelProvider {

  // interface methods of LabelProvider
  /////////////////////////////////////
  
  @Override
  public String getText( final Object element ) {
    IImportLibrary library = ( IImportLibrary )element;
    return library.getPath().toOSString();
  }

  @Override
  public Image getImage( final Object element ) {
    String key = ISharedImages.IMG_OBJ_FOLDER;
    return PlatformUI.getWorkbench().getSharedImages().getImage( key );
  }
}