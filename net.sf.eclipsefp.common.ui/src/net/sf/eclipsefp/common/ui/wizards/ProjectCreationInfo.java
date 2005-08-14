// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.wizards;

import org.eclipse.jface.resource.ImageDescriptor;


/** <p>contains info needed by the ProjectCreationWizard and its helpers
  * for creating a new project. This is filled and passed by the special
  * fp classes that want to create projects with certain natures (Scheme 
  * projects, Haskell projects etc.)</p>
  * 
  * @author Leif Frenzel
  */
public class ProjectCreationInfo {

  // UI texts and images
  private ImageDescriptor bannerImage;
  private String pageTitle;
  private String pageDescription;
  
  /** the project natures that the creates project will have. */
  private String[] projectNatures;
  /** the name for the project that is created. */
  private String projectName;
  /** directories to create in the project's directory. */
  private String[] directories;
  /** information about the descriptor file to be created. */
  private DescriptorFileInfo descFileInfo;


  // attribute setters and getters
  ////////////////////////////////

  protected void setBannerImage( final ImageDescriptor bannerImage ) {
    this.bannerImage = bannerImage;
  }
  
  ImageDescriptor getBannerImage() {
    return bannerImage;
  }
  
  protected void setPageDescription( final String pageDescription ) {
    this.pageDescription = pageDescription;
  }
  
  String getPageDescription() {
    return pageDescription;
  }

  protected void setPageTitle( final String pageTitle ) {
    this.pageTitle = pageTitle;
  }
  
  String getPageTitle() {
    return pageTitle;
  }

  protected void setProjectNatures( final String[] projectNatures ) {
    this.projectNatures = projectNatures;
  }
  
  String[] getProjectNatures() {
    return projectNatures;
  }

  String getProjectName() {
    return projectName;
  }

  void setProjectName( final String projectName ) {
    this.projectName = projectName;
  }
  
  String[] getDirectories() {
    return directories;
  }

  protected void setDirectories( final String[] directories ) {
    this.directories = directories;
  }
  
  DescriptorFileInfo getDescFileInfo() {
    return descFileInfo;
  }

  protected void setDescFileInfo( final DescriptorFileInfo descFileInfo ) {
    this.descFileInfo = descFileInfo;
  }
}