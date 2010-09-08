// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.views.common;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;


/** <p>Action for opening a Haskell compilation unit etc., typically after
  * double click on a viewer item.</p>
  *
  * @author Leif Frenzel
  */
public class OpenViewerElement extends Action {

  private final Object element;

  public OpenViewerElement( final DoubleClickEvent event ) {
    Viewer viewer = event.getViewer();
    ISelection selection = viewer.getSelection();
    element = ( ( IStructuredSelection )selection ).getFirstElement();
  }


  // interface methods of IAction
  ///////////////////////////////

  @Override
  public void run() {
    IResource resource = ResourceUtil.findResource( element );
    if( resource instanceof IFile ) {
      if( FileUtil.hasHaskellExtension( resource ) ) {
        openEditor( ( IFile )resource );
      }
    }
  }


  // helping methods
  //////////////////

  private static void openEditor( final IFile file ) {
    IEditorInput input = new FileEditorInput( file );
    try {
      getPage().openEditor( input, HaskellEditor.ID, true );
    } catch( PartInitException ex ) {
      String path = file.getFullPath().toString();
      HaskellUIPlugin.log( "Could not open editor for " + path + ".", ex );
    }
  }

  private static IWorkbenchPage getPage() {
    return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
  }
}