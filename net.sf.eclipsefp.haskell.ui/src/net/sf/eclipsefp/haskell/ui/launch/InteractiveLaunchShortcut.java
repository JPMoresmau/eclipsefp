// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.launch;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;

import de.leiffrenzel.fp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;

/** <p>Shortcut to launch interactive sessions from the 'Run' action set.</p>
  * 
  * <p>Subclasses must be declared in the <code>plugin.xml</code> and then 
  * need only provide an implementation of 
  * <code>IInteractiveLaunchOperationDelegate</code> that knows about 
  * specific details.</p>
  * 
  * @author Leif Frenzel
  */
public abstract class InteractiveLaunchShortcut implements ILaunchShortcut {

  // methods to be implemented by subclasses
  //////////////////////////////////////////
  
  /** <p>returns the delegate that knows about the specific details
    * for launching an interactive environment.</p> */
  public abstract IInteractiveLaunchOperationDelegate getDelegate();
  


  // interface methods of ILaunchShortcut
  ///////////////////////////////////////

  public void launch( final ISelection selection, final String mode ) {
    // launched from workbench selection
    if( selection != null && selection instanceof IStructuredSelection ) {
      List list = new ArrayList();
      IStructuredSelection ssel = ( IStructuredSelection )selection;
      Iterator iter = ssel.iterator();  
      while( iter.hasNext() ) {
        Object element = iter.next();
        IResource res = ResourceUtil.findResource( element );
        if( res != null ) {
         list.add( res );
        }
      }
      IResource[] ress = toArray( list );
      launch( ress );
    }
  }

  public void launch( final IEditorPart editor, final String mode ) {
    // launched from editor part
    IResource resource = ResourceUtil.findResource( editor.getEditorInput() );
    launch( new IResource[] { resource } );
  }
  
  
  // helping methods
  //////////////////

  private void launch( final IResource[] resources ) {
    // TODO put this into a Job and use the progress monitor
    // also: need a public job family in core (with icon in ui)
    try {
      IProgressMonitor monitor = new NullProgressMonitor();
      new InteractiveLaunchOperation( getDelegate() ).launch( resources, 
                                                              monitor );
    } catch( CoreException cex ) {
      // TODO show msg box
      HaskellUIPlugin.log( "Could not launch Haskell application.", cex ); 
    }
  }
  
  private IResource[] toArray( final List list ) {
    IResource[] result = new IResource[ list.size() ];
    list.toArray( result );
    return result;
  }
}