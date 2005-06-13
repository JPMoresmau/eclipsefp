// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.haddock.HaddockPlugin;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;

/** <p>content provider for the list part of the viewer on the selection 
  * page.</p>
  *
  * @author Leif Frenzel
  */
public class CheckboxListCP implements IStructuredContentProvider {
  
  
  // interface methods of IStructuredContentProvider
  //////////////////////////////////////////////////
  
  public Object[] getElements( final Object inputElement ) {
    Object[] result = new Object[ 0 ];
    if( inputElement instanceof IContainer ) {
      result = getHaskellFiles( ( IContainer )inputElement );
    }
    return result;
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
  
  private Object[] getHaskellFiles( final IContainer container ) {
    Object[] result = new Object[ 0 ];
    try {
      IResource[] ress = container.members();
      List list = new ArrayList();
      for( int i = 0; i < ress.length; i++ ) {
        if( ress[ i ] instanceof IFile
            && ResourceUtil.hasHaskellExtension( ress[ i ] ) ) {
          list.add( ress[ i ] );
        }
      }
      result = list.toArray();
    } catch( CoreException cex ) {
      HaddockPlugin.log( cex.toString(), cex ); 
    }
    return result;
  }
}
