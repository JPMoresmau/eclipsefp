// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// Copyright (c) 2011 by Alejandro Serrano
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.ProjectExplorerStanza;
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

 public abstract List<ILaunchConfiguration> findConfiguration(IProject project,final PackageDescriptionStanza stanza) throws CoreException;

 public abstract IExecutableTestSuiteLaunchOperation getLaunchOperation();

 //interface methods of ILaunchShortcut
 ///////////////////////////////////////

 public void launch( final ISelection selection, final String mode ) {
   // launched from workbench selection
   if( selection instanceof IStructuredSelection ) {
     Object element = ( ( IStructuredSelection )selection ).getFirstElement();
     if (element instanceof ProjectExplorerStanza){
       ProjectExplorerStanza st=(ProjectExplorerStanza)element;
       launch(st.getOwner(),st.getStanza());
     } else {
       launch( ResourceUtil.findResource( element ),null );
     }
   }
 }

 public void launch( final IEditorPart editor, final String mode ) {
   // launched from editor part
   launch( ResourceUtil.findResource( editor.getEditorInput() ),null );
 }


 // helping methods
 //////////////////

 private void launch( final IResource resource, final PackageDescriptionStanza stanza ) {
   // TODO put this in a Job and use the progress monitor
   try {
     getLaunchOperation().launch( resource, null,stanza );
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
     List<ILaunchConfiguration> cs=findConfiguration( resource.getProject(),null );
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
       PackageDescriptionStanza stanza=findStanza( paramISelection );
       IResource[] res=ResourceUtil.getResourcesFromSelection( paramISelection ) ;
       if (res.length>0){
         List<ILaunchConfiguration> cs=findConfiguration(res[0].getProject(),stanza);
         return cs.toArray( new ILaunchConfiguration[cs.size()] );
       }
   } catch (CoreException cex){
     HaskellUIPlugin.log( cex );
   }
   return null;
 }

   private static PackageDescriptionStanza findStanza(final ISelection paramISelection){
     if (paramISelection instanceof IStructuredSelection){
       IStructuredSelection sel=(IStructuredSelection)paramISelection;
       if (sel.size()==1){
         return findStanza( sel.getFirstElement() );
       }
     }
     return null;
   }

   private static PackageDescriptionStanza findStanza(final Object o){
     if (o instanceof ProjectExplorerStanza){
       return ((ProjectExplorerStanza)o).getStanza();
     }
     if (o instanceof PackageDescriptionStanza){
       return (PackageDescriptionStanza)o;
     }
     return null;
   }
}
