// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.decorators;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;


/** <p>decorates project folders in Haskell projects.</p>
  *
  * @author Leif Frenzel
  */
public class ProjectFoldersDecorator extends LabelProvider
                                     implements ILabelDecorator {


  // interface methods of ILabelDecorator
  ///////////////////////////////////////

  @Override
  public Image decorateImage( final Image baseImage, final Object element ) {
    // if it remains null, this means no special decoration
    Image result = null;
    IFolder folder = getFolder( element );
    if( folder != null ) {
        IProject project = folder.getProject();
        if(   ResourceUtil.hasHaskellNature( project )) {
          result = decorate( folder,baseImage );
        }
    }
    return result;
  }

  @Override
  public String decorateText( final String text, final Object element ) {
    // no special text decorations
    return text;
  }


  // helping methods
  //////////////////

  private IFolder getFolder( final Object element ) {
    IFolder result = null;
    if( element instanceof IFolder ) {
      result = ( IFolder )element;
    } else if( element instanceof IAdaptable ) {
      Object adapter = ( ( IAdaptable )element ).getAdapter( IFolder.class );
      if( adapter instanceof IFolder ) {
        result = ( IFolder )adapter;
      }
    }
    return result;
  }

  private Image decorate( final IFolder folder,
                          final Image baseImage ) {
    Image result = null;
    if( ResourceUtil.isSourceFolder( folder ) )  {
      result = getImage( baseImage, IImageNames.SRC_FOLDER_DECORATOR );
    }
    return result;
  }

  private Image getImage( final Image baseImage, final String name ) {
    ImageData data = getImageData( name );
    DecoratorImageDescriptor did = new DecoratorImageDescriptor( baseImage,
                                                                 data );
    return did.createImage();
  }

  private ImageData getImageData( final String name ) {
    return HaskellUIImages.getImageDescriptor( name ).getImageData();
  }
}