// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.code;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.IPath;


/** <p>holds detailed information about the module which is to be created
  * in the 'New Module' wizard.</p>
  *
  * @author Leif Frenzel
  */
public class ModuleCreationInfo {

  private String moduleName = ""; //$NON-NLS-1$
  private IPath folders;
  private IContainer sourceContainer;
  private EHaskellCommentStyle fCommentStyle = EHaskellCommentStyle.USUAL;


  // attribute setters and getters
  ////////////////////////////////

  public IPath getFolders() {
    return folders;
  }

  public void setFolders( final IPath folders ) {
    this.folders = folders;
  }

  public String getModuleName() {
    return this.moduleName;
  }

  public void setModuleName( final String moduleName ) {
    this.moduleName = moduleName;
  }

  public IContainer getSourceContainer() {
    return sourceContainer;
  }

  public void setSourceContainer( final IContainer sourceContainer ) {
    this.sourceContainer = sourceContainer;
  }

  public EHaskellCommentStyle getCommentStyle() {
    return fCommentStyle;
  }

  public void setCommentStyle( final EHaskellCommentStyle style ) {
    fCommentStyle = style;
  }
}