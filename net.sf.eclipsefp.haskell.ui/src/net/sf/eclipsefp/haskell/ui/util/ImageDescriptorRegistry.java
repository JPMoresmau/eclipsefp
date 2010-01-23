// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;


/** </p>A registry that maps <code>ImageDescriptors</code> to
  * <code>Image</code>.</p>
  *
  * @author Leif Frenzel
  */
class ImageDescriptorRegistry {

  private final Map<ImageDescriptor, Image> registry;
  private final Display display;


  /** <p>creates a new image descriptor registry for the given display.
    * All images managed by this registry will be disposed when the display
    * gets disposed.</p> */
  public ImageDescriptorRegistry() {
    this.registry = new HashMap<ImageDescriptor, Image>();
    this.display = HaskellUIPlugin.getStandardDisplay();
    hookDisplay();
  }

  public Image get( final ImageDescriptor descriptor ) {
    ImageDescriptor desc = descriptor;
    if( desc == null ) {
      desc = ImageDescriptor.getMissingImageDescriptor();
    }

    Image result = registry.get( desc );
    if( result == null ) {
      Assert.isTrue( display == HaskellUIPlugin.getStandardDisplay(),
                     "Allocating image for wrong display." ); //$NON-NLS-1$
      result = desc.createImage();
      if( result != null ) {
        registry.put( desc, result );
      }
    }
    return result;
  }

  public void dispose() {
    Iterator iter = registry.values().iterator();
    while( iter.hasNext() ) {
      ( ( Image )iter.next() ).dispose();
    }
    registry.clear();
  }


  // helping methods
  //////////////////

  private void hookDisplay() {
    display.disposeExec( new Runnable() {
      public void run() {
        dispose();
      }
    } );
  }
}