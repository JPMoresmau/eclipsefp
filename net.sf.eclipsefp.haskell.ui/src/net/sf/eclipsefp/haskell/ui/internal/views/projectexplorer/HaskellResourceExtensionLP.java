// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer;

import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.project.IImportLibrary;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;
import net.sf.eclipsefp.haskell.ui.internal.views.outline.OutlineLabelProvider;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.graphics.Image;

/** <p>the label provider for elements in a Haskell project. Functionality
  * for language elements is inherited.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellResourceExtensionLP extends OutlineLabelProvider {


  // interface methods
  ////////////////////

  @Override
  public String getText( final Object element ) {
    String result;
    if( element instanceof IHaskellProject ) {
      result = ( ( IHaskellProject )element ).getResource().getName();
    } else if( element instanceof ITreeElement ) {
      result = ( ( ITreeElement )element ).getText();
    } else if( element instanceof IFolder ) {
      result = ( ( IFolder )element ).getName();
    } else if( element instanceof IFile ) {
      result = ( ( IResource )element ).getName();
    } else if( element instanceof ProjectExplorerOutlineDef ) {
      result =super.getText(( ( ProjectExplorerOutlineDef )element ).getOutlineDef());
    } else {
      result = super.getText( element );
    }
    return result;
  }

  @Override
  public Image getImage( final Object element ) {
    Image result = null;
    if( element instanceof ITreeElement ) {
      String key = ( ( ITreeElement )element ).getImageKey();
      result = HaskellUIImages.getImage( key );
    } else if( element instanceof IHaskellProject ) {
      result = HaskellUIImages.getImage( IImageNames.HASKELL_PROJECT );
    } else if( element instanceof IImportLibrary ) {
      result = HaskellUIImages.getImage( IImageNames.IMPORT_LIBRARY );
    } else if( element instanceof IFolder ) {
      result = getFolderImage( ( IFolder )element );
    } else if( element instanceof IFile ) {
      result = getFileImage( ( IFile )element );
    } else if( element instanceof ProjectExplorerOutlineDef ) {
      result =super.getImage(( ( ProjectExplorerOutlineDef )element ).getOutlineDef());
    } else {
      result=super.getImage( element );
    }
    return result;
  }


  // helping methods
  //////////////////

  private Image getFileImage( final IFile file ) {
    Image result = null;
    try {
      if( file.getProject().hasNature( HaskellNature.NATURE_ID ) ) {
        //if( ResourceUtil.isProjectExecutable( file ) ) {
        //  result = HaskellUIImages.getImage( IImageNames.PROJECT_EXECUTABLE );
        //} else {
          String ext = file.getFileExtension();
          if( ResourceUtil.EXTENSION_HS.equals( ext ) ) {
            result = HaskellUIImages.getImage( IImageNames.SOURCE_FILE );
          } else if( ResourceUtil.EXTENSION_LHS.equals( ext ) ) {
            result = HaskellUIImages.getImage( IImageNames.LITERATE_SOURCE_FILE );
          }
        //}
      }
    } catch( final CoreException cex ) {
      HaskellUIPlugin.log( cex );
    }
    return result;
  }

  private Image getFolderImage( final IFolder folder ) {
    Image result = null;
    try {
      if(    folder.getProject().hasNature( HaskellNature.NATURE_ID )
          && ResourceUtil.isSourceFolder( folder ) ) {
        String id = IImageNames.SOURCE_FOLDER;
        result = HaskellUIImages.getImage( id );
      }
    } catch( final CoreException cex ) {
      HaskellUIPlugin.log(  cex );
    }
    return result;
  }
}
