package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import org.eclipse.ui.editors.text.TextEditor;


public class AlexEditor extends TextEditor {

  public AlexEditor() {
    // Do nothing by way
  }

  @Override
  protected void initializeEditor() {
    super.initializeEditor();
    this.setSourceViewerConfiguration( new AlexSourceViewerConfiguration( this ) );
  }
}
