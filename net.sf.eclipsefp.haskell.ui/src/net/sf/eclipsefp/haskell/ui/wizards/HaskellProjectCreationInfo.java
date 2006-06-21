// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.common.ui.wizards.ProjectCreationInfo;

import org.eclipse.jface.resource.ImageDescriptor;

import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;

/** <p>A project creation info for the "New Project" wizard with Haskell 
  * project specific settings.</p> 
  * 
  * @author Leif Frenzel
  */
public class HaskellProjectCreationInfo extends ProjectCreationInfo {

  public HaskellProjectCreationInfo() {
    setPageTitle( "Haskell project" ); 
    setPageDescription( "Create a new Haskell project in the workspace." );
    
    applyBannerImage();
  }


  // helping methods
  //////////////////

  private void applyBannerImage() {
    String bannerImage = IImageNames.NEW_PROJECT;
    ImageDescriptor desc = HaskellUIImages.getImageDescriptor( bannerImage );
    setBannerImage( desc );
  }

}