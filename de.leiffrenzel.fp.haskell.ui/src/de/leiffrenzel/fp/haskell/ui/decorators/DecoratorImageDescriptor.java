// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.decorators;

import org.eclipse.jface.resource.CompositeImageDescriptor;
import org.eclipse.swt.graphics.*;


/** <p>an image descriptor for decorators. This means we have a composite 
  * image involvong an overlay image.</p>
  * 
  * @author Leif Frenzel
  */
class DecoratorImageDescriptor extends CompositeImageDescriptor {

  private Image baseImage;
  private ImageData overlayImageData;
  
  DecoratorImageDescriptor( final Image baseImage, 
                            final ImageData overlayImageData ) {
    this.baseImage = baseImage;
    // TODO this is ugly
    this.overlayImageData = overlayImageData;
  }

  protected void drawCompositeImage( final int width, final int height ) {
    drawImage( baseImage.getImageData(), 0, 0 );  
    int xValue = baseImage.getBounds().width - overlayImageData.width; 
    int yValue = 0; 
    drawImage( overlayImageData, xValue, yValue );  
  }

  protected Point getSize() {
    return new Point( baseImage.getBounds().width,
                      baseImage.getBounds().height );
  }
}