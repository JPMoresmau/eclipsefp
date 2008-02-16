// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview;

import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.project.IProjectPropertiesEvent;
import net.sf.eclipsefp.haskell.core.project.IProjectPropertiesListener;

import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Control;


/** <p>listens for resource changes and refreshes the Module Browser,
  * if necessary.</p>
  *
  * @author Leif Frenzel
  */
class Refresher implements IResourceChangeListener, IProjectPropertiesListener {

  private TreeViewer viewer;
  private final ModuleBrowser moduleBrowser;

  Refresher( final ModuleBrowser moduleBrowser ) {
    this.moduleBrowser = moduleBrowser;
  }


  void setViewer( final TreeViewer viewer ) {
    this.viewer = viewer;
    // TODO event mask
    ResourcesPlugin.getWorkspace().addResourceChangeListener( this );
    HaskellProjectManager.addProjectPropertiesListener( this );
  }

  public void dispose() {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );
    HaskellProjectManager.removeProjectPropertiesListener( this );
  }


  // interface methods of IProjectPropertiesListener
  //////////////////////////////////////////////////

  public void projectPropertyChanged( final IProjectPropertiesEvent event ) {
    IHaskellProject hsProject = event.getSource();
    viewer.refresh( hsProject );
  }


  // interface methods of IResourceChangeListener
  ///////////////////////////////////////////////

  public final void resourceChanged( final IResourceChangeEvent event ) {
    Control ctrl = viewer.getControl();
    if( ctrl != null && !ctrl.isDisposed() ) {
      // Do a sync exec, not an async exec, since the resource delta
      // must be traversed in this method. It is destroyed
      // when this method returns.
      ctrl.getDisplay().syncExec( new Runnable() {
        public void run() {
          triggerRefresh();
        }
      } );
    }
  }


  // helping methods
  //////////////////

  // TODO must also listen from property changes: libraries, project executable

  // TODO look at WorkbenchContentProvider
  private void triggerRefresh() {
    // This method runs inside a syncExec. The widget may have been destroyed
    // by the time this is run. Check for this and do nothing if so.
    Control ctrl = viewer.getControl();
      if( ctrl != null && !ctrl.isDisposed() ) {
      // TODO not sure how to handle this
      //  -  check only Haskell projects (nothing else is displayed
      moduleBrowser.refreshViewer();
    }
  }
}