// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring.actions;


import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.RefInfo;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;

abstract class RefAction {

  private IEditorPart targetEditor;
  RefInfo info = new RefInfo();
  ISelection selection;
  boolean haveFile;

  abstract void openWizard();


  // interface methods of IEditorActionDelegate
  /////////////////////////////////////////////

  public void run( final IAction action ) {
    if( !haveFile ) {
      refuse();
    } else {
      if( selection != null && selection instanceof ITextSelection ) {
        applySelection( ( ITextSelection )selection );
        if( saveAll() ) {
          openWizard();
        }
      }
    }
  }

  public void setActiveEditor( final IAction action, final IEditorPart editor ) {
    this.targetEditor = editor;
    haveFile = false;
    if( getFile() != null ) {
      haveFile = true;
    }
  }

  public void selectionChanged( final IAction action, final ISelection selection ) {
    this.selection = selection;
  }


  // helping functions
  ////////////////////

  private void applySelection( final ITextSelection textSelection ) {
    info.setOffset( textSelection.getOffset() );
    int start = textSelection.getStartLine();
    info.setLine( start );
    info.setColumn( computeColumn( start, textSelection.getOffset() ) );
    info.setText( textSelection.getText() );
    info.setSourceFile( getFile() );
  }

  private int computeColumn( final int start, final int offset ) {
    int result = 0;
    if( targetEditor instanceof HaskellEditor ) {
      try {
        HaskellEditor haskellEditor = ( HaskellEditor )targetEditor;
        IDocument doc = haskellEditor.getDocument();
        result = offset - doc.getLineOffset( start );
      } catch( BadLocationException ex ) {
        // TODO Auto-generated catch block
        ex.printStackTrace();
      }
    }
    return result;
  }

  private void refuse() {
    // TODO lf gen
    String title = UITexts.mkPointFree_refuseDlg_title;
    String message = UITexts.mkPointFree_refuseDlg_message;
    MessageDialog.openInformation( getShell(), title, message );
  }

  Shell getShell() {
    Shell result = null;
    if( targetEditor != null ) {
      result = targetEditor.getSite().getShell();
    } else {
      result = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    }
    return result;
  }

  private IFile getFile() {
    IFile result = null;
    if( targetEditor instanceof ITextEditor )  {
      ITextEditor editor = ( ITextEditor )targetEditor;
      IEditorInput input = editor.getEditorInput();
      if( input instanceof IFileEditorInput ) {
        result = ( ( IFileEditorInput )input ).getFile();
      }
    }
    return result;
  }

  private static boolean saveAll() {
    IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
    return IDE.saveAllEditors( new IResource[] { workspaceRoot }, false );
  }
}