package net.sf.eclipsefp.haskell.ui.internal.editors.uuagcOptions;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.forms.editor.FormEditor;


public class UuagcOptionsFormEditor extends FormEditor {

  private TextEditor sourceEditor;

  @Override
  protected void addPages() {
    try {
      sourceEditor = new TextEditor();
      addPage( sourceEditor, getEditorInput() );
      setPageText( 0, UITexts.uuagcEditor_source );
    } catch( PartInitException ex ) {
      HaskellUIPlugin.log( "Unable to create form pages.", ex ); //$NON-NLS-1$
    }
  }

  @Override
  public void doSave( final IProgressMonitor monitor ) {
    if( sourceEditor != null ) {
      sourceEditor.doSave( monitor );
    }
  }

  @Override
  public void doSaveAs() {
    if( sourceEditor != null ) {
      sourceEditor.doSaveAs();
    }
  }

  @Override
  public boolean isSaveAsAllowed() {
    boolean result = false;
    if( sourceEditor != null ) {
      result = sourceEditor.isSaveAsAllowed();
    }
    return result;
  }

}
