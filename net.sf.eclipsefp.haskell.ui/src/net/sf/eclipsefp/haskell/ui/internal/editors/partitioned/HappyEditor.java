/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

/**
 * Text editor for Happy parsers.
 * @author Alejandro Serrano
 *
 */
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
