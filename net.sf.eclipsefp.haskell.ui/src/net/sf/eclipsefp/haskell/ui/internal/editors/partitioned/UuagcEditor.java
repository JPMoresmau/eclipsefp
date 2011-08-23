package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

public class UuagcEditor extends PartitionEditor {

  public UuagcEditor() {
    // Do nothing by way
  }

  @Override
  protected void initializeEditor() {
    super.initializeEditor();
    this.setSourceViewerConfiguration( new UuagcSourceViewerConfiguration( this ) );
  }
}
