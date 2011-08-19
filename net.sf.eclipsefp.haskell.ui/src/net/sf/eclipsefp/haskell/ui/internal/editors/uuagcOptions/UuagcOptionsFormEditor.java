package net.sf.eclipsefp.haskell.ui.internal.editors.uuagcOptions;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.forms.editor.FormEditor;


public class UuagcOptionsFormEditor extends FormEditor {

  private UuagcOptionsPage optionsPage;
  private UuagcTextEditor sourceEditor;

  public IDocument getModel() {
    IDocument result = null;
    if( sourceEditor != null ) {
      result = sourceEditor.getDocument();
    }
    return result;
  }

  @Override
  protected void addPages() {
    try {
      IFileEditorInput input = (IFileEditorInput)getEditorInput();
      optionsPage = new UuagcOptionsPage( this, input.getFile().getProject() );
      addPage( optionsPage );
      sourceEditor = new UuagcTextEditor();
      addPage( sourceEditor, input );
      setPageText( 1, UITexts.uuagcEditor_source );
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
