// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.expressions;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.*;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;


/** <p>For plugins that have to check some properties of elements in
  * the workspace, e.g. whether they are the project executable or the
  * source folder of a Haskell project.</p>
  * 
  * <p>This is used in the XML Expression language.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellPropertyTester extends PropertyTester {

  
  // interface methods of PropertyTester
  //////////////////////////////////////
  
  public boolean test( final Object receiver, 
                       final String property, 
                       final Object[] args,
                       final Object expectedValue ) {
    IResource resource = ( IResource )receiver;
    boolean result = false;
    if( property.equals( "isProjectExecutable" ) ) {
      result =    resource instanceof IFile
               && ResourceUtil.isInHaskellProject( resource )
               && ResourceUtil.isProjectExecutable( ( IFile )resource );
    } else if( property.equals( "isSourceFolder" ) ) {
      result =    resource instanceof IFolder
               && ResourceUtil.isInHaskellProject( resource )
               && ResourceUtil.isSourceFolder( ( IFolder )resource );
    }
    return result;
  }
}