// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mdepview;

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.*;

import net.sf.eclipsefp.haskell.core.halamo.Halamo;
import net.sf.eclipsefp.haskell.core.halamo.IModule;

/** <p>the content provider for the dependencies view.</p>
  *
  * @author Leif Frenzel 
  */
class ModuleDependenciesContentProvider implements IStructuredContentProvider,
                                                   ITreeContentProvider {

  private static final Object[] EMPTY = new Object[ 0 ]; 
  
  public Object[] getElements( final Object inputElement ) {
    Object[] result = EMPTY; 
    if( inputElement instanceof IProject ) {
      IProject project = ( IProject )inputElement;
      result = Halamo.getInstance().getAllModules( project );
    }
    // else?? module?? source folder??
    return result;
  }
    
  public Object getParent( final Object child ) {
    // TODO ??
    return null;
  }
  
  public Object [] getChildren( final Object parent ) {
    Object[] result = EMPTY;
    if( parent instanceof IModule ) {
      result = ( ( IModule )parent ).getImports();
    }
    return result;
  }
  
  public boolean hasChildren( final Object parent ) {
    boolean result = false;
    if( parent instanceof IModule ) {
      result = ( ( IModule )parent ).getImports().length > 0;
    }
    return result;
  }
    
  public void inputChanged( final Viewer viewer, 
                            final Object oldInput, 
                            final Object newInput ) {
    // TODO
  }
    
  public void dispose() {
    // unused
  }
}