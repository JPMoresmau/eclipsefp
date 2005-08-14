// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.dialog;

import org.eclipse.swt.widgets.*;


/** <p>a dialog field that allows the selection of a directory.</p>
  * 
  * @author Leif Frenzel
  */
public class FileDialogField extends BrowseDialogField {

  private final String[] filterExtensions;


  public FileDialogField( final Composite parent, 
                          final String labelText, 
                          final String[] filterExtensions ) {
    super( parent, labelText );
    this.filterExtensions = filterExtensions;
  }
  
  
  // interface methods of BrowseDialogField
  /////////////////////////////////////////
  
  String openDialog( final Shell shell ) {
    FileDialog dialog = new FileDialog( shell );
    dialog.setText( "Select a file" );
    dialog.setFilterExtensions( filterExtensions );
    return dialog.open();
  }
}