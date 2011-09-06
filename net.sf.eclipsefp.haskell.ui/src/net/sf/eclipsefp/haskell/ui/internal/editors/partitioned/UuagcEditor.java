/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

/**
 * Text editor for UUAGC files.
 * @author Alejandro Serrano
 *
 */
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
