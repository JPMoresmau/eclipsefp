/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.List;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.ui.ILaunchShortcut2;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;


/**
 * Shortcut for "cabal test"
 * @author JP Moresmau
 *
 */
public class CabalTestLaunchShortcut implements ILaunchShortcut2 {

  /**
   *
   */
  public CabalTestLaunchShortcut() {
  }

  /* (non-Javadoc)
   * @see org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.jface.viewers.ISelection, java.lang.String)
   */
  @Override
  public void launch( final ISelection selection, final String mode ) {
 // launched from workbench selection
    if( selection instanceof IStructuredSelection ) {
      Object element = ( ( IStructuredSelection )selection ).getFirstElement();
      launch( ResourceUtil.findResource( element ));
    }

  }

  /* (non-Javadoc)
   * @see org.eclipse.debug.ui.ILaunchShortcut#launch(org.eclipse.ui.IEditorPart, java.lang.String)
   */
  @Override
  public void launch( final IEditorPart editor, final String mode ) {
    launch( ResourceUtil.findResource( editor.getEditorInput() ));

  }

  private void launch( final IResource resource) {
    // TODO put this in a Job and use the progress monitor
    try {
      getLaunchOperation().launch( resource, null );
    } catch( CoreException cex ) {
      // TODO show msg box
      String msg = "Could not launch Haskell application."; //$NON-NLS-1$
      HaskellUIPlugin.log( msg, cex );
    }
  }

  protected CabalTestLaunchOperation getLaunchOperation(){
    return new CabalTestLaunchOperation();
  }

  /* (non-Javadoc)
   * @see org.eclipse.debug.ui.ILaunchShortcut2#getLaunchConfigurations(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public ILaunchConfiguration[] getLaunchConfigurations( final ISelection selection ) {
    try {
      IResource[] res=ResourceUtil.getResourcesFromSelection( selection ) ;
      if (res.length>0){
        List<ILaunchConfiguration> cs=getLaunchOperation().findConfiguration(res[0].getProject());
        return cs.toArray( new ILaunchConfiguration[cs.size()] );
      }
  } catch (CoreException cex){
    HaskellUIPlugin.log( cex );
  }
  return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.debug.ui.ILaunchShortcut2#getLaunchConfigurations(org.eclipse.ui.IEditorPart)
   */
  @Override
  public ILaunchConfiguration[] getLaunchConfigurations( final IEditorPart editorpart ) {
    IResource resource = ResourceUtil.findResource( editorpart.getEditorInput() );
    try {
      List<ILaunchConfiguration> cs=getLaunchOperation().findConfiguration( resource.getProject() );
      return cs.toArray( new ILaunchConfiguration[cs.size()] );
    } catch (CoreException cex){
      HaskellUIPlugin.log( cex );
    }
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.debug.ui.ILaunchShortcut2#getLaunchableResource(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public IResource getLaunchableResource( final ISelection selection ) {
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.debug.ui.ILaunchShortcut2#getLaunchableResource(org.eclipse.ui.IEditorPart)
   */
  @Override
  public IResource getLaunchableResource( final IEditorPart editorpart ) {
    return null;
  }

}
