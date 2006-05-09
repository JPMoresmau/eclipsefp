// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.common.ui.wizards.DescriptorFileInfo;
import net.sf.eclipsefp.common.ui.wizards.ProjectCreationInfo;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;

import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;

/** <p>A project creation info for the "New Project" wizard with Haskell 
  * project specific settings.</p> 
  * 
  * @author Leif Frenzel
  */
class HaskellProjectCreationInfo extends ProjectCreationInfo {

  HaskellProjectCreationInfo() {
    setPageTitle( "Haskell project" ); 
    setPageDescription( "Create a new Haskell project in the workspace." );
    setProjectNatures( new String[] { HaskellNature.NATURE_ID } );
    setDirectories( new String[ 0 ] );
    
    applyDescFileInfo();
    applyBannerImage();
  }


  // helping methods
  //////////////////

  private void applyBannerImage() {
    String bannerImage = IImageNames.NEW_PROJECT;
    ImageDescriptor desc = HaskellUIImages.getImageDescriptor( bannerImage );
    setBannerImage( desc );
  }

  private void applyDescFileInfo() {
    String content;
    if( createFolders() ) {
      String sourcePath = getPref( IPreferenceConstants.FOLDERS_SRC );
      String outputPath = getPref( IPreferenceConstants.FOLDERS_OUT );
      String binPath = getPref( IPreferenceConstants.FOLDERS_BIN );
      String[] dirs = new String[] { sourcePath, outputPath, binPath };
      setDirectories( dirs );
      
      String targetBinary = getPref( IPreferenceConstants.TARGET_BINARY );
      content = HaskellProjectManager.createDescriptorContent( sourcePath,
                                                               outputPath,
                                                               binPath,
                                                               targetBinary );
    } else {
      content = HaskellProjectManager.createEmptyDescriptorContent();
    }
    String name = HaskellProjectManager.HASKELL_PROJECT_DESCRIPTOR;
    DescriptorFileInfo descFileInfo = new DescriptorFileInfo( name, content );
    setDescFileInfo( descFileInfo );
  }


  private String getPref( final String name ) {
    return getStore().getString( name );
  }

  private IPreferenceStore getStore() {
    return HaskellUIPlugin.getDefault().getPreferenceStore();
  }

  private boolean createFolders() {
    String name = IPreferenceConstants.FOLDERS_IN_NEW_PROJECT;
    return getStore().getBoolean( name );
  }
}