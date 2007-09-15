// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.actions;

import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.views.mdepview.ModuleDependenciesView;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;


/** <p>Workbench Action to open the Module Dependencies view (with some 
  * specific input).</p>
  * 
  * @author Leif Frenzel
  */
public class OpenDependenciesView implements IWorkbenchWindowActionDelegate {
  
  private ISelection selection;

  // interface methods of IWorkbenchWindowActionDelegate
  //////////////////////////////////////////////////////
  
  public void run( final IAction action ) {
    if( selection instanceof IStructuredSelection ) {
      IStructuredSelection strucSel = ( IStructuredSelection )selection;
      openDependencies( strucSel.getFirstElement() );
    }
  }

  public void dispose() {
    // unused
  }

  public void init( final IWorkbenchWindow window ) {
    // unused
  }

  public void selectionChanged( final IAction action, 
                                final ISelection selection ) {
    this.selection = selection;
  }
  
  
  // helping methods
  //////////////////
  
  private void openDependencies( final Object element ) {
    if( element instanceof IProject ) {
      IProject project = ( IProject )element;
      try {
        openDependencies( project );
      } catch( CoreException ex ) {
        HaskellUIPlugin.log( "Could not open Module Dependencies View.", ex );
      }
    }
  }
  
  private void openDependencies( final IProject project ) throws CoreException {
    Assert.isTrue( project.hasNature( HaskellNature.NATURE_ID ) );
    IWorkbenchPage page = getPage();
    try {
      IViewPart view = page.showView( ModuleDependenciesView.ID );
      ( ( ModuleDependenciesView )view ).openTo( project ); 
    } catch( PartInitException piex ) {
      throw new CoreException( piex.getStatus() );
    }
  }

  private IWorkbenchPage getPage() {
    return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
  }
}