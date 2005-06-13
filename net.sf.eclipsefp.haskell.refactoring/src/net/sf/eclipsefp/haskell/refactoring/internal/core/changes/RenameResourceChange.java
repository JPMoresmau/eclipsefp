// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.core.changes;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;

/** <p>a change object for renaming resources.</p>
  *
  * <p>taken and refactored from <code>
  * org.eclipse.jdt.internal.corext.refactoring.changes.RenameResourceChange
  * </code></p>
  *
  * @author Leif Frenzel
  */
public class RenameResourceChange extends BaseChange {

  // can't use handles because they become invalid when the resource is renamed
  private IPath resourcePath;
  private String newName;
  private long stampToRestore;

  /**
   * @param newName includes the extension
   */
  public RenameResourceChange( final IResource resource, 
                               final String newName ) {
    this( resource.getFullPath(), newName, IResource.NULL_STAMP );
  }

  private RenameResourceChange( final IPath resourcePath, 
                                final String newName,
                                final long stampToRestore ) {
    this.resourcePath = resourcePath;
    this.newName = newName;
    this.stampToRestore = stampToRestore;
  }
  

  public RefactoringStatus isValid( final IProgressMonitor pm ) 
                                                          throws CoreException {
    IResource resource = getResource();
    RefactoringStatus result;
    if( resource == null || !resource.exists() ) {
      String msg = "'" + resourcePath.toString() + "' does not exist";
      result = RefactoringStatus.createFatalErrorStatus( msg );
    } else {
      result = super.isValid( pm, false, true );
    }
    return result;
  }
  
  
  // interface methods of Change
  //////////////////////////////
  
  // to avoid the exception senders should check if a resource with the new name
  public Change perform( final IProgressMonitor mon ) throws CoreException {
    try {
      mon.beginTask( "rename resource", 1 );
      IResource resource = getResource();
      long currentStamp = resource.getModificationStamp();
      IPath newPath = renamedResourcePath( resourcePath, newName );
      resource.move( newPath, getCoreRenameFlags(), mon );
      if( stampToRestore != IResource.NULL_STAMP ) {
        IWorkspace workspace = ResourcesPlugin.getWorkspace();
        IResource newResource = workspace.getRoot().findMember( newPath );
        newResource.revertModificationStamp( stampToRestore );
      }
      String oldName = resourcePath.lastSegment();
      return new RenameResourceChange( newPath, oldName, currentStamp );
    } finally {
      mon.done();
    }
  }

  public String getName() {
    return "Rename '" + resourcePath.toString() + "' to: '" + newName + "'";
  }

  public Object getModifiedElement() {
    return getResource();
  }
  
  
  // helping methods
  //////////////////
  
  private int getCoreRenameFlags() {
    return getResource().isLinked() ? IResource.SHALLOW : IResource.NONE;
  }
  
  private IResource getResource() {
    return ResourcesPlugin.getWorkspace().getRoot().findMember( resourcePath );
  }
  
  // changes resource names /s/p/A.java renamed to B.java becomes /s/p/B.java
  private IPath renamedResourcePath( final IPath path, 
                                           final String newName ) {
    return path.removeLastSegments( 1 ).append( newName );
  }
}