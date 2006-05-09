// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.builder;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.IProgressMonitor;

/** <p>Visits the resource tree to build Haskell projects.</p>
  * 
  * @author Leif Frenzel
  */
class BuildVisitor extends Visitor implements IResourceVisitor {

  BuildVisitor( final IProgressMonitor monitor ) {
    super( monitor );
  }
  
  public boolean visit( final IResource res ) {
    //build the specified resource.
    //return true to continue visiting children.
    if( res instanceof IFile ) {
      IFile file = ( IFile )res;
      if( isHaskellFile( file ) ) {
        compileFile( file );
      }
    }
    return true;
  }
}