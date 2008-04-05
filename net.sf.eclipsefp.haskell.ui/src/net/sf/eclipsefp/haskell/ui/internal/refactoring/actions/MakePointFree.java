// Copyright (c) 2007-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.refactoring.actions;

import net.sf.eclipsefp.haskell.ui.internal.refactoring.MakePointFreeInfo;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.MakePointFreeProcessor;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.MakePointFreeRefactoring;
import net.sf.eclipsefp.haskell.ui.internal.refactoring.wizards.MakePointFreeWizard;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;

/** <p>an action for triggering the PointFree-refactoring on a text
  * selection.</p>
  *
  * <p>This action is declared in the <code>plugin.xml</code>.</p>
  *
  * @author Leif Frenzel
  */
public class MakePointFree implements IEditorActionDelegate {

  private ISelection selection;
  private IEditorPart targetEditor;
  private boolean haveFile;

  private final MakePointFreeInfo info = new MakePointFreeInfo();


  // interface methods of IEditorActionDelegate
  /////////////////////////////////////////////

  public void setActiveEditor( final IAction action,
                               final IEditorPart targetEditor ) {
    this.targetEditor = targetEditor;
    haveFile = false;
    if( getFile() != null ) {
      haveFile = true;
    }
  }

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

  public void selectionChanged( final IAction action,
                                final ISelection selection ) {
    this.selection = selection;
  }


  // helping methods
  //////////////////

  private void applySelection( final ITextSelection textSelection ) {
    info.setOffset( textSelection.getOffset() );
    info.setText( textSelection.getText() );
    info.setSourceFile( getFile() );
  }

  private void refuse() {
    String title = UITexts.mkPointFree_refuseDlg_title;
    String message = UITexts.mkPointFree_refuseDlg_message;
    MessageDialog.openInformation( getShell(), title, message );
  }

  private static boolean saveAll() {
    IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
    return IDE.saveAllEditors( new IResource[] { workspaceRoot }, false );
  }

  private void openWizard() {
    RefactoringProcessor processor = new MakePointFreeProcessor( info );
    MakePointFreeRefactoring ref = new MakePointFreeRefactoring( processor );
    MakePointFreeWizard wizard = new MakePointFreeWizard( ref );
    RefactoringWizardOpenOperation op
      = new RefactoringWizardOpenOperation( wizard );
    try {
      String titleForFailedChecks = ""; //$NON-NLS-1$
      op.run( getShell(), titleForFailedChecks );
    } catch( final InterruptedException irex ) {
      // operation was cancelled
    }
  }

  private Shell getShell() {
    Shell result = null;
    if( targetEditor != null ) {
      result = targetEditor.getSite().getShell();
    } else {
      result = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
    }
    return result;
  }

  private final IFile getFile() {
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
}

