/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

/**
 * Text editor for Alex lexers.
 * @author Alejandro Serrano
 *
 */
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
