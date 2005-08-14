// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.dialog;

import org.eclipse.swt.widgets.*;


/** <p>a dialog field that allows the selection of a directory.</p>
  * 
  * @author Leif Frenzel
  */
public class DirectoryDialogField extends BrowseDialogField {

  public DirectoryDialogField( final Composite parent, 
                               final String labelText ) {
    super( parent, labelText );
  }
  
  
  // interface methods of BrowseDialogField
  /////////////////////////////////////////
  
  String openDialog( final Shell shell ) {
    DirectoryDialog dialog = new DirectoryDialog( shell );
    dialog.setText( "Select a directory" );
    return dialog.open();
  }
}