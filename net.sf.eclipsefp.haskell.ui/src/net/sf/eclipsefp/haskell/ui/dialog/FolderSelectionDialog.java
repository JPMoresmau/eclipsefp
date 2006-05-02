// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.dialog;

import org.eclipse.core.resources.IContainer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;


/** <p>a dialog that allows the user to select from all folders in
  * a Haskell project, starting from a given source folder.</p>
  * 
  * @author Leif Frenzel
  */
public class FolderSelectionDialog extends ElementTreeSelectionDialog {

  public FolderSelectionDialog( final Shell shell, 
                                final IContainer rootContainer ) {
    super( shell, new DialogLabelProvider(), new FolderCP() );
    setInput( rootContainer );
    setTitle( "Folder Selection" );
    setMessage( "Choose a folder:" );
    setEmptyListMessage( "No folder to select." );
  }
}