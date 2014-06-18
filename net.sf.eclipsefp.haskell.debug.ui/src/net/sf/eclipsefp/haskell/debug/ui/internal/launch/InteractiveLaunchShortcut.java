// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.IInteractiveLaunchOperationDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.ui.ILaunchShortcut2;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;

/** <p>Shortcut to launch interactive sessions from the 'Run' action set.</p>
  *
  * <p>Subclasses must be declared in the <code>plugin.xml</code> and then
  * need only provide an implementation of
  * <code>IInteractiveLaunchOperationDelegate</code> that knows about
  * specific details.</p>
  *
  * @author Leif Frenzel
  */
public abstract class InteractiveLaunchShortcut implements ILaunchShortcut2 {

  // methods to be implemented by subclasses
  //////////////////////////////////////////

  /** <p>returns the delegate that knows about the specific details
    * for launching an interactive environment.</p> */
  public abstract IInteractiveLaunchOperationDelegate getDelegate();



  // interface methods of ILaunchShortcut
  ///////////////////////////////////////

  @Override
  public void launch( final ISelection selection, final String mode ) {
    // launched from workbench selection
    if( selection != null && selection instanceof IStructuredSelection ) {
      List<IResource> list = new ArrayList<>();
      IStructuredSelection ssel = ( IStructuredSelection )selection;
      for( Object element: ssel.toList() ) {
        IResource res = ResourceUtil.findResource( element );
        if( res != null ) {
         list.add( res );
        }
      }
      IResource[] ress = ResourceUtil.toResourceArray( list );
      launch( ress,mode );
    }
  }



  @Override
  public void launch( final IEditorPart editor, final String mode ) {
    // launched from editor part
    IResource resource = ResourceUtil.findResource( editor.getEditorInput() );
    launch( new IResource[] { resource } ,mode);
  }


  // helping methods
  //////////////////

  protected void launch( final IResource[] resources,final String mode ) {
    // TODO put this into a Job and use the progress monitor
    // also: need a public job family in core (with icon in ui)
    try {
      IProgressMonitor monitor = new NullProgressMonitor();
      new InteractiveLaunchOperation( getDelegate() ).launch( resources,mode,
                                                              monitor );
    } catch( CoreException cex ) {
      // TODO show msg box
      String msg = "Could not launch Haskell application."; //$NON-NLS-1$
      HaskellUIPlugin.log( msg, cex );
    }
  }


  @Override
  public IResource getLaunchableResource( final IEditorPart paramIEditorPart ) {
    return null;
  }
  @Override
  public IResource getLaunchableResource( final ISelection paramISelection ) {
    return null;
  }
  @Override
  public ILaunchConfiguration[] getLaunchConfigurations(
      final IEditorPart paramIEditorPart ) {
    IResource resource = ResourceUtil.findResource( paramIEditorPart.getEditorInput() );
    try {
      List<ILaunchConfiguration> cs=InteractiveLaunchOperation.findConfig( getDelegate(),new IResource[]{ resource },getConfigTypeName() );
      return cs.toArray( new ILaunchConfiguration[cs.size()] );
  } catch (CoreException cex){
    HaskellUIPlugin.log( cex );
  }
  return null;
  }

  /**
   * configuration type name. Subclasess can override if they use a different type but want ot piggy back on this implementation
   * @return
   */
  protected String getConfigTypeName() {
    return InteractiveLaunchOperation.INTERACTIVE_CONFIG_TYPE;
  }

  /**
   * this allows launching a new configuration
   */
  @Override
  public ILaunchConfiguration[] getLaunchConfigurations(
      final ISelection paramISelection ) {
    try {
        List<ILaunchConfiguration> cs=InteractiveLaunchOperation.findConfig( getDelegate(), ResourceUtil.getResourcesFromSelection( paramISelection ),getConfigTypeName()  );
        return cs.toArray( new ILaunchConfiguration[cs.size()] );
    } catch (CoreException cex){
      HaskellUIPlugin.log( cex );
    }
    return null;
  }

}