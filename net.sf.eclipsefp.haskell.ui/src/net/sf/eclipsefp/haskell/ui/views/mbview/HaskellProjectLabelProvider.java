// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.graphics.Image;

import de.leiffrenzel.fp.haskell.core.halamo.IHaskellLanguageElement;
import de.leiffrenzel.fp.haskell.core.project.IHaskellProject;
import de.leiffrenzel.fp.haskell.core.project.IImportLibrary;
import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import net.sf.eclipsefp.haskell.ui.views.common.HaskellLabelProvider;


/** <p>the label provider for elements in a Haskell project. Functionality
  * for language elements is inherited.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellProjectLabelProvider extends HaskellLabelProvider
                                         implements IImageNames {

  private UIState uiState;

  HaskellProjectLabelProvider( final UIState uiState ) {
    this.uiState = uiState;
  }
  
  
  // interface methods
  ////////////////////
  
  public String getText( final Object element ) {
    String result;
    if( element instanceof IHaskellProject ) {
      result = ( ( IHaskellProject )element ).getResource().getName();
    } else if( element instanceof IFolder ) {
      result = getFolderText( ( IFolder )element );
    } else if( element instanceof IFile ) {
      result = ( ( IResource )element ).getName();
    } else {
      result = super.getText( element );
    }
    return result;
  }
  
  public Image getImage( final Object element ) {
    Image result = null;
    if( element instanceof IHaskellLanguageElement ) {
      result = super.getImage( element );
    } else if( element instanceof IHaskellProject ) {
      result = HaskellUIImages.getImage( HASKELL_PROJECT );
    } else if( element instanceof IImportLibrary ) {
      result = HaskellUIImages.getImage( IMPORT_LIBRARY );
    } else if( element instanceof IFolder ) {
      result = getFolderImage( ( IFolder )element );
    } else if( element instanceof IFile ) {
      result = getFileImage( ( IFile )element );
    }
    return result;
  }
  
  
  // helping methods
  //////////////////

  private Image getFileImage( final IFile file ) {
    String id;
    // this is either the project executable or some Haskell source file
    if( ResourceUtil.isProjectExecutable( file ) ) {
      id = PROJECT_EXECUTABLE; 
    } else {
      String ext = file.getFileExtension();
      if( ext.equals( ResourceUtil.EXTENSION_HS ) ) {
        id = SOURCE_FILE;
      } else if( ext.equals( ResourceUtil.EXTENSION_LHS ) ) {
        id = LITERATE_SOURCE_FILE;
      } else {
        // other resources in the source folder
        // TODO
        id = IImageNames.MODULE;        
      }
    }
    return HaskellUIImages.getImage( id );
  }
  
  private Image getFolderImage( final IFolder folder ) {
    // this is either the source folder or some package fragment
    String id = ResourceUtil.isSourceFolder( folder ) ? SOURCE_FOLDER : PACKAGE;
    return HaskellUIImages.getImage( id );
  }
  
  private String getFolderText( final IFolder folder ) {
    String result = folder.getName();
    if(    !uiState.isFlatLayout() 
        && Util.applyHierarchicalLayout( folder ) ) {
      try {
        IFolder subFolder = ( IFolder )folder.members()[ 0 ];
        result = result + "." + getFolderText( subFolder );
      } catch( CoreException cex ) {
        String msg = "Problem with children of '" + folder.getName() + "'.";
        HaskellUIPlugin.log( msg, cex );
      }
    }
    return result;
  }
}