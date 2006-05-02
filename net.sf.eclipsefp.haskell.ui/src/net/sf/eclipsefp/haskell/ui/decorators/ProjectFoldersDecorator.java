// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.decorators;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.*;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;

import de.leiffrenzel.fp.haskell.core.project.*;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;


/** <p>decorates project folders in Haskell projects.</p>
  * 
  * @author Leif Frenzel
  */
public class ProjectFoldersDecorator extends LabelProvider  
                                     implements ILabelDecorator {


  // interface methods of ILabelDecorator
  ///////////////////////////////////////

  public Image decorateImage( final Image baseImage, final Object element ) {
    // if it remains null, this means no special decoration
    Image result = null;
    IFolder folder = getFolder( element );
    if( folder != null ) {
      try {
        IProject project = folder.getProject();
        if( project.hasNature( HaskellNature.NATURE_ID ) ) {
          result = decorate( folder.getProjectRelativePath(), 
                             HaskellProjectManager.get( project ),
                             baseImage );
        }
      } catch( CoreException cex ) {
        HaskellUIPlugin.log( "Could not decorate Haskell project folders.", 
                             cex );
      }
    }
    return result;
  }

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
  
  private Image decorate( final IPath folderPath, 
                          final IHaskellProject hsProject,
                          final Image baseImage ) {
    Image result = null;
    if( folderPath.equals( hsProject.getBinPath() ) ) {
      result = getImage( baseImage, IImageNames.BIN_FOLDER_DECORATOR );
    } else if( folderPath.equals( hsProject.getSourcePath() ) ) {
      result = getImage( baseImage, IImageNames.SRC_FOLDER_DECORATOR );
    }
    return result;
  }

  private Image getImage( final Image baseImage, final String name ) {
    // TODO use ImageRegistry from the plugin
    ImageData data = getImageData( name );
    DecoratorImageDescriptor did = new DecoratorImageDescriptor( baseImage, 
                                                                 data );
    return did.createImage(); 
  }
  
  private ImageData getImageData( final String name ) {
    return HaskellUIImages.getImageDescriptor( name ).getImageData();
  } 
}