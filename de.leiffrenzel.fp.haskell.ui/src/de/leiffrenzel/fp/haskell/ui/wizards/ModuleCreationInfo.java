// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.wizards;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.IPath;


/** <p>holds detailed information about the module which is to be created  
  * in the 'New Module' wizard.</p>
  * 
  * @author Leif Frenzel
  */
class ModuleCreationInfo {

  private String moduleName = "";
  private IPath folders;
  private IContainer sourceContainer;
  
  
  // attribute setters and getters
  ////////////////////////////////
  
  IPath getFolders() {
    return folders;
  }

  void setFolders( final IPath folders ) {
    this.folders = folders;
  }

  String getModuleName() {
    return this.moduleName;
  }

  void setModuleName( final String moduleName ) {
    this.moduleName = moduleName;
  }
  
  IContainer getSourceContainer() {
    return sourceContainer;
  }
 
  void setSourceContainer( final IContainer sourceContainer ) {
    this.sourceContainer = sourceContainer;
  }
}