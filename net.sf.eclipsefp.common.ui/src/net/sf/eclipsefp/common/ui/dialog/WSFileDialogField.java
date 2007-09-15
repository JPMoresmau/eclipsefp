// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.dialog;

import org.eclipse.core.resources.*;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.views.navigator.ResourceComparator;

/** <p>a dialog field that allows the selection of a directory.</p>
 * 
 * @author Leif Frenzel
 */
public class WSFileDialogField extends BrowseDialogField {

  private final String extension;

  public WSFileDialogField( final Composite parent, final String labelText ) {
    super( parent, labelText );
    extension = null;
  }

  public WSFileDialogField( final Composite parent, 
                            final String labelText,
                            final String extension ) {
    super( parent, labelText );
    this.extension = extension;
  }

  
  // interface methods of BrowseDialogField
  /////////////////////////////////////////

  @Override
  String openDialog( final Shell shell ) {
    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    // TODO initial selection
    //  IResource focus= initialSelection != null ?
    // root.findMember(initialSelection) : null;
    ElementTreeSelectionDialog dialog = createDialog( shell );
    dialog.setTitle( "Select a file" );
    addFilter( dialog );
    dialog.setInput( root );
    dialog.setComparator( new ResourceComparator( ResourceComparator.NAME ) );
    //  dialog.setInitialSelection(focus);
    return openDialog( dialog );
  }

  
  // helping methods
  //////////////////
  
  private String openDialog( final ElementTreeSelectionDialog dialog ) {
    String result = "";
    if( dialog.open() == Window.OK ) {
      Object[] elements = dialog.getResult();
      if( elements.length > 0 ) {
        IResource res = ( IResource )elements[ 0 ];
        result = res.getLocation().toOSString();
      }
    }
    return result;
  }

  private ElementTreeSelectionDialog createDialog( final Shell shell ) {
    WorkbenchLabelProvider lp = new WorkbenchLabelProvider();
    WorkbenchContentProvider cp = new WorkbenchContentProvider();
    return new ElementTreeSelectionDialog( shell, lp, cp );
  }

  private void addFilter( final ElementTreeSelectionDialog dialog ) {
    if( extension != null ) {
      dialog.addFilter( new ViewerFilter() {
        @Override
        public boolean select( final Viewer viewer, 
                               final Object parentElement,
                               final Object element ) {
          boolean result = true;
          IResource res = ( IResource )element;
          if( res instanceof IFile ) {
            result = res.getFileExtension().equals( extension );
          }
          return result;
        }
      } );
    }
  }
}