package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.editors.text.TextEditor;

public class PartitionEditor extends TextEditor {

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

}