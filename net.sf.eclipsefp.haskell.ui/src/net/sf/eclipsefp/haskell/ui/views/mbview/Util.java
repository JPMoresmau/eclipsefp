// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;


/** <p>provides static helping functionality for analyzing relations 
  * between objects in the Module Browser.</p>
  * 
  * @author Leif Frenzel
  */
public class Util {

  static boolean applyHierarchicalLayout( final IFolder folder ) {
    boolean result = false;
    if( !ResourceUtil.isSourceFolder( folder ) ) {
      try {
        IResource[] members = folder.members();
        if(    members.length == 1 
            && members[ 0 ].getType() == IResource.FOLDER ) {
          result = true;
        }
      } catch( CoreException ex ) {
        String msg = "Problem with childrens of folder " + folder;
        HaskellUIPlugin.log( msg, ex );
      }
    }
    return result;
  }
}