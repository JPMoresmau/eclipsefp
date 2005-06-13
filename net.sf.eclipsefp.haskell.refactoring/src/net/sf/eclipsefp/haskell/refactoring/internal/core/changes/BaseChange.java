// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.core.changes;

import org.eclipse.core.filebuffers.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;

/** <p>superclass for changes, encapsulates common functionality.</p>
  * 
  * <p>taken and refactored from <code>
  * org.eclipse.jdt.internal.corext.refactoring.base.JDTChange
  * </code></p>
  * 
  * @author Leif Frenzel
  */
abstract class BaseChange extends Change {

  // TODO there is more checking meanwhile in JDTChange
  
  private long modificationStamp;

  
  // interface methods of Change
  // ////////////////////////////

  public void initializeValidationData( final IProgressMonitor mon ) {
    IResource resource = getResource( getModifiedElement() );
    if( resource != null ) {
      modificationStamp = resource.getModificationStamp();
    }
  }


  // interface methods of Object
  // ////////////////////////////

  public String toString() {
    return getName();
  }

  
  // helping methods
  // ////////////////

  RefactoringStatus isValid( final IProgressMonitor mon,
                             final boolean checkReadOnly, 
                             final boolean checkDirty ) {
    mon.beginTask( "", 1 );
    RefactoringStatus result = new RefactoringStatus();
    IResource resource = getResource( getModifiedElement() );
    if( resource != null ) {
      checkIfModifiable( result, resource, checkReadOnly, checkDirty );
      checkModificationStamp( result, resource );
    }
    mon.worked( 1 );
    return result;
  }

  void checkModificationStamp( final RefactoringStatus status,
                               final IResource resource ) {
    if( modificationStamp != IResource.NULL_STAMP
        && modificationStamp != resource.getModificationStamp() ) {
      String msg =   resource.getFullPath().toString() 
                   + " has been modified since the refactoring got executed.";
      status.addFatalError( msg );
    }
  }

  void checkIfModifiable( final RefactoringStatus status,
                          final IResource resource, 
                          final boolean checkReadOnly,
                          final boolean checkDirty ) {
    if( checkReadOnly ) {
      checkReadOnly( status, resource );
    }
    if( checkDirty ) {
      checkIfDirty( status, resource );
    }
  }

  void checkReadOnly( final RefactoringStatus status, final IResource res ) {
    if( isReadOnly( res ) ) {
      status.addFatalError( res.getFullPath().toString() + " is read only." );
    }
  }

  void checkIfDirty( final RefactoringStatus status, final IResource res ) {
    if( res instanceof IFile ) {
      IFile file = ( IFile )res;
      if( file.exists() ) {
        ITextFileBufferManager man = FileBuffers.getTextFileBufferManager();
        ITextFileBuffer buffer = man.getTextFileBuffer( file.getFullPath() );
        if( buffer != null && buffer.isDirty() ) {
          String msg = file.getFullPath().toString() + " is unsaved.";
          status.addFatalError( msg );
        }
      }
    }
  }
  
  private static IResource getResource( final Object element ) {
    IResource result = null;
    if( element instanceof IResource ) {
      result = ( IResource )element;
    } else if( element instanceof IAdaptable ) {
      IAdaptable adaptable = ( IAdaptable )element;
      result = ( IResource )adaptable.getAdapter( IResource.class );
    }
    return result;
  }
  
  private boolean isReadOnly( final IResource resource ) {
    ResourceAttributes resourceAttributes = resource.getResourceAttributes();
    boolean result = false;
    // not supported on this platform for this resource
    if( resourceAttributes != null ) {
      result = resourceAttributes.isReadOnly();
    }
    return result;
  }
}