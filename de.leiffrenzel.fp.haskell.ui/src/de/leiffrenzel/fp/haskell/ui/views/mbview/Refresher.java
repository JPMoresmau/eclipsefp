// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.views.mbview;

import org.eclipse.core.resources.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.widgets.Control;

import de.leiffrenzel.fp.haskell.core.project.*;
import de.leiffrenzel.fp.haskell.core.project.IProjectPropertiesEvent;
import de.leiffrenzel.fp.haskell.core.project.IProjectPropertiesListener;


/** <p>listens for resource changes and refreshes the Module Browser, 
  * if necessary.</p>
  * 
  * @author Leif Frenzel
  */
class Refresher implements IResourceChangeListener, IProjectPropertiesListener {

  private TreeViewer viewer;
  private ModuleBrowser moduleBrowser;

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
    final IResourceDelta delta = event.getDelta();
    Control ctrl = viewer.getControl();
    if( ctrl != null && !ctrl.isDisposed() ) {
      // Do a sync exec, not an async exec, since the resource delta
      // must be traversed in this method. It is destroyed
      // when this method returns.
      ctrl.getDisplay().syncExec( new Runnable() {
        public void run() {
          processDelta( delta );
        }
      } );
    }
  }

  
  // helping methods
  //////////////////

  // TODO must also listen from property changes: libraries, project executable
  
  // TODO look at WorkbenchContentProvider
  private void processDelta( final IResourceDelta delta ) {
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