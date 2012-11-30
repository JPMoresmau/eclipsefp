// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.dialog;

import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;


/** <p>a dialog that allows the user to select from all source folders in
  * all Haskell projects.</p>
  *
  * @author Leif Frenzel
  */
public class SourceFolderSelectionDialog extends ElementTreeSelectionDialog {

  public SourceFolderSelectionDialog( final Shell shell ) {
//   this( shell, ResourcesPlugin.getWorkspace().getRoot(),false);
    super( shell, new DialogLabelProvider(), new SourceFolderCP() );
    setValidator( new SourceFolderValidator() );
    setInput( ResourcesPlugin.getWorkspace().getRoot() );
    setTitle( UITexts.source_folder_title );
    setMessage(UITexts.source_folder_msg);
  }



}