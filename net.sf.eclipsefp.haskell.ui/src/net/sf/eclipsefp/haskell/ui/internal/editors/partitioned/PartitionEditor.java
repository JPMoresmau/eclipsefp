/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.TabChecker;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ScionTokenScanner;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;

/**
 * Base editor for Alex, Happy and UUAGC text editors.
 * @author Alejandro Serrano
 *
 */
public class PartitionEditor extends TextEditor {

  private ScionTokenScanner tokenScanner;



  public PartitionEditor() {
    super();
  }

  /**
   * Get the editor's current input file.
   *
   * @return An IFile object if the editor's input is a file, otherwise null.
   */
  public IFile findFile() {
    IEditorInput input = getEditorInput();
    if( input instanceof IFileEditorInput ) {
      return ( ( IFileEditorInput )input ).getFile();
    }
    return null;
  }

  public IDocument getDocument() {

    IDocumentProvider docProvider = getDocumentProvider();
    if (docProvider!=null){
      return docProvider.getDocument( getEditorInput() );
    }
    return null;
  }

  @Override
  protected void doSetInput( final IEditorInput input ) throws CoreException {
    super.doSetInput( input );
    new TabChecker(this);
  }

  @Override
  protected void editorSaved() {
    if (tokenScanner!=null){
      tokenScanner.markTaskTags();
    }
    super.editorSaved();
  }


  public ScionTokenScanner getTokenScanner() {
    return tokenScanner;
  }


  public void setTokenScanner( final ScionTokenScanner tokenScanner ) {
    this.tokenScanner = tokenScanner;
  }

}