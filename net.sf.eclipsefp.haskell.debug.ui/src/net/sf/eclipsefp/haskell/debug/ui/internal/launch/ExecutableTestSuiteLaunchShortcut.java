// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.List;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.ui.ILaunchShortcut2;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;

/**
 * Base class for actions in the 'Run' menu.
 * @author Alejandro Serrano
 *
 */
public abstract class ExecutableTestSuiteLaunchShortcut implements ILaunchShortcut2 {

 public abstract List<ILaunchConfiguration> findConfiguration(IProject project) throws CoreException;

 public abstract IExecutableTestSuiteLaunchOperation getLaunchOperation();

 //interface methods of ILaunchShortcut
 ///////////////////////////////////////

 public void launch( final ISelection selection, final String mode ) {
   // launched from workbench selection
   if( selection instanceof IStructuredSelection ) {
     Object element = ( ( IStructuredSelection )selection ).getFirstElement();
     launch( ResourceUtil.findResource( element ) );
   }
 }

 public void launch( final IEditorPart editor, final String mode ) {
   // launched from editor part
   launch( ResourceUtil.findResource( editor.getEditorInput() ) );
 }


 // helping methods
 //////////////////

 private void launch( final IResource resource ) {
   // TODO put this in a Job and use the progress monitor
   try {
     getLaunchOperation().launch( resource, null );
   } catch( CoreException cex ) {
     // TODO show msg box
     String msg = "Could not launch Haskell application."; //$NON-NLS-1$
     HaskellUIPlugin.log( msg, cex );
   }
 }

 public IResource getLaunchableResource( final IEditorPart paramIEditorPart ) {
   return null;
 }
 public IResource getLaunchableResource( final ISelection paramISelection ) {
   return null;
 }
 public ILaunchConfiguration[] getLaunchConfigurations(
     final IEditorPart paramIEditorPart ) {
   IResource resource = ResourceUtil.findResource( paramIEditorPart.getEditorInput() );
   try {
     List<ILaunchConfiguration> cs=findConfiguration( resource.getProject() );
     return cs.toArray( new ILaunchConfiguration[cs.size()] );
 } catch (CoreException cex){
   HaskellUIPlugin.log( cex );
 }
 return null;
 }

 /**
  * this allows launching a new configuration
  */
 public ILaunchConfiguration[] getLaunchConfigurations(
     final ISelection paramISelection ) {
   try {
       IResource[] res=ResourceUtil.getResourcesFromSelection( paramISelection ) ;
       if (res.length>0){
         List<ILaunchConfiguration> cs=findConfiguration(res[0].getProject());
         return cs.toArray( new ILaunchConfiguration[cs.size()] );
       }
   } catch (CoreException cex){
     HaskellUIPlugin.log( cex );
   }
   return null;
 }

}
