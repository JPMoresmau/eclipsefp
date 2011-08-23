package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

public class AlexEditor extends PartitionEditor {

  public AlexEditor() {
    // Do nothing by way
  }

  @Override
  protected void initializeEditor() {
    super.initializeEditor();
    this.setSourceViewerConfiguration( new AlexSourceViewerConfiguration( this ) );
  }
}
