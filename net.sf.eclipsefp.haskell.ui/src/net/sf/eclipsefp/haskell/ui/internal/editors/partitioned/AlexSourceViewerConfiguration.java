package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;


public class AlexSourceViewerConfiguration extends SourceViewerConfiguration
    implements IEditorPreferenceNames {

  /** The associated editor */
  final AlexEditor editor;
  /** The plugin's preference store */
  private IPreferenceStore prefStore;

  /**
   * The constructor
   *
   * @param editor
   *          The associated Haskell editor
   */
  public AlexSourceViewerConfiguration( final AlexEditor editor ) {
    this.editor = editor;
  }


  @Override
  public int getTabWidth( final ISourceViewer sourceViewer ) {
    return getPreferenceStore().getInt( EDITOR_TAB_WIDTH );
  }

  private IPreferenceStore getPreferenceStore() {
    if( prefStore != null ) {
      return prefStore;
    }
    return HaskellUIPlugin.getDefault().getPreferenceStore();
  }

  public void setPreferenceStore( final IPreferenceStore prefStore ) {
    this.prefStore = prefStore;
  }
}
