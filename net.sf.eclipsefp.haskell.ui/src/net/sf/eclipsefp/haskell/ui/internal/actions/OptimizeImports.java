// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.actions;

import net.sf.eclipsefp.haskell.core.internal.code.IOrganizeImports;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

/** <p>action for triggering the import optimization on an editor buffer.</p>
  *
  * @author Leif Frenzel
  */
public class OptimizeImports implements IEditorActionDelegate {

  private IEditorPart editor;


  // interface methods of IEditorActionDelegate
  /////////////////////////////////////////////

  public void run( final IAction action ) {
    Shell shell = editor.getSite().getShell();
    MessageDialog.openInformation(
      shell,
      "Not yet implemented",
      "This functionality is not yet implemented." );

//    IDocument document = getDocument();
//    if( document != null ) {
//      String content = document.get();
//      IOrganizeImports oi = loadFunction();
//      if( oi != null ) {
//        try {
//          String result = oi.organizeImports( content );
//          if( result != null ) {
//            document.set( result );
//          }
//        } catch( final CohatoeException cex ) {
//          HaskellUIPlugin.log( cex );
//        }
//      }
//    }
  }

  public void selectionChanged( final IAction action, final ISelection sel ) {
    // unused
  }

  public void setActiveEditor( final IAction act, final IEditorPart editor ) {
    this.editor = editor;
  }


  // helping functions
  ////////////////////

  private IOrganizeImports loadFunction() {
    return CohatoeServer.getInstance().createFunction( IOrganizeImports.class );
  }

  private IDocument getDocument() {
    IDocument result = null;
    if( editor instanceof HaskellEditor ) {
      result = ( ( HaskellEditor )editor ).getDocument();
    }
    return result;
  }
}
