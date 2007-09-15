// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.dialog;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;


/** content provider for a dialog that shows only subfolders of a specified
  * folder.
  * 
  * @author Leif Frenzel
  */
class FolderCP implements ITreeContentProvider {

  // interface methods of ITreeContentProvider
  ////////////////////////////////////////////
  
  public Object[] getChildren( final Object parentElement ) {
    IResource[] members = getMembers( parentElement );
    List<IResource> list = new ArrayList<IResource>();
    for( int i = 0; i < members.length; i++ ) {
      IResource member = members[ i ];
      if( member.exists() && member instanceof IFolder ) {
        list.add( member );
      }
    }
    return list.toArray();
  }

  public Object getParent( final Object element ) {
    return null;
  }

  public boolean hasChildren( final Object element ) {
    return getChildren( element ).length > 0;
  }

  public Object[] getElements( final Object inputElement ) {
    return getChildren( inputElement );
  }

  public void dispose() {
    // unused
  }

  public void inputChanged( final Viewer viewer, 
                            final Object oldInput, 
                            final Object newInput ) {
    // unused
  }
  
  
  // helping methods
  //////////////////
  
  private IResource[] getMembers( final Object parentElement ) {
    IContainer container = ( IContainer )parentElement;
    IResource[] members = new IResource[ 0 ];
    try {
      members = container.members();
    } catch( CoreException ex ) {
      HaskellUIPlugin.getDefault().getLog().log( ex.getStatus() );
    }
    return members;
  }
}