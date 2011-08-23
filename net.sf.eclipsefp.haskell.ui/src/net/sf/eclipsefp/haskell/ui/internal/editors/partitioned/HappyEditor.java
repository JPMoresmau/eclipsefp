package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

public class HappyEditor extends PartitionEditor {

  public HappyEditor() {
    // Do nothing by way
  }

  @Override
  protected void initializeEditor() {
    super.initializeEditor();
    this.setSourceViewerConfiguration( new HappySourceViewerConfiguration( this ) );
  }
}
