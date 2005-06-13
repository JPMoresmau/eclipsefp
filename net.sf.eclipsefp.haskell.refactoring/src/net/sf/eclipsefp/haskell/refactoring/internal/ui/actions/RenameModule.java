// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.refactoring.internal.ui.actions;

import net.sf.eclipsefp.haskell.refactoring.internal.ui.HaskellRefactoringUI;
import net.sf.eclipsefp.haskell.refactoring.internal.ui.wizards.RenameModuleInfo;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.ui.*;
import org.eclipse.ui.texteditor.ITextEditor;

/** <p>an action to trigger a Rename refactoring for a module. This can be run
  * from an editor, from a view or from the workbench.</p>
  *
  * @author Leif Frenzel
  */
public class RenameModule implements IEditorActionDelegate {

  // TODO make sure it runs from the wb, from the Navigator, and from the 
  // Module Browser
  
  private IWorkbenchWindow window;
  private IEditorPart editorPart;
  private RenameModuleInfo info;
  
  
  // interface methods of IEditorActionDelegate
  /////////////////////////////////////////////
  
  public void run( final IAction action ) {
    if( window != null ) {
      // need to re-evaluate selection
      if( editorPart != null ) {
        setEditorPart( editorPart );
      }
      if( info != null ) {
        HaskellRefactoringUI.getDefault().rename( window.getShell(), info );
      }
    }
  }
  
  public void setActiveEditor( final IAction action, 
                               final IEditorPart targetEditor ) {
    info = null;
    window = null;
    if( targetEditor != null ) {
      window = targetEditor.getSite().getWorkbenchWindow();
      setEditorPart( targetEditor );
    }
    updateEnablement( action );    
  }

  public void selectionChanged( final IAction action, 
                                final ISelection selection ) {
    info = null;
    editorPart = null;
    if( window != null ) {
      IEditorPart editorPart = window.getActivePage().getActiveEditor();
      if( editorPart == window.getActivePage().getActivePart() ) {
        setEditorPart( editorPart );
      }
    }
    updateEnablement( action );
  }

  
  // helping methods
  //////////////////
  
  private void updateEnablement( final IAction action ) {
    action.setEnabled( window != null && info != null );
  }
  
  private void setEditorPart( final IEditorPart editorPart ) {
    if( editorPart instanceof ITextEditor ) {
      ITextEditor textEditor = ( ITextEditor )editorPart;
      ISelectionProvider provider = textEditor.getSelectionProvider();
      if( provider != null ) {
        ISelection selection = provider.getSelection();
        if( selection instanceof ITextSelection ) {
          ITextSelection textSelection = ( ITextSelection )selection;
          info = new RenameModuleInfo( textEditor, textSelection );
          this.editorPart = editorPart;
        }
      }
    }
  }
}
