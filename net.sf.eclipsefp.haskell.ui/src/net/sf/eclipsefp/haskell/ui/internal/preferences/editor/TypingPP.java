package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;

/**
 *
 * @deprecated
 * @author JP Moresmau
 */
@Deprecated
public class TypingPP extends AbstractEditorPP {
//  private DialogField spaceForTabs;

  @Override
  protected void addPreferences( final OverlayPreferenceStore store ) {
   // store.addBooleanKey( EDITOR_SPACES_FOR_TABS );
    store.addBooleanKey( EDITOR_CLOSE_STRINGS );
    store.addBooleanKey( EDITOR_CLOSE_BRACKETS_AND_PARENS );
    store.addBooleanKey( EDITOR_CLOSE_BRACES );
    store.addIntKey( EDITOR_TAB_WIDTH );
    store.addIntKey( EDITOR_CABAL_TAB_WIDTH );

  }

  @Override
  public void init( final IWorkbench workbench ) {
    setDescription(  UITexts.preferences_editor_typing_title );
    super.init( workbench );
  }

  @Override
  protected Control createContents( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );

    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    composite.setLayout( layout );

//    String prefName = IEditorPreferenceNames.EDITOR_SPACES_FOR_TABS;
//    spaceForTabs=createBooleanField( composite, UITexts.preferences_editor_typing_spaces_tabs, prefName );
//    new Label(composite,SWT.NONE);

    tab.addIntegerField( composite, UITexts.preferences_editor_typing_tab_width, IEditorPreferenceNames.EDITOR_TAB_WIDTH, 3, 0 );
    tab.addIntegerField( composite, UITexts.preferences_editor_typing_cabal_tab_width, IEditorPreferenceNames.EDITOR_CABAL_TAB_WIDTH, 3, 0 );

    tab.initializeFields();

    return composite;
  }

  public void propertyChange( final PropertyChangeEvent event ) {
    /*if (spaceForTabs!=null){
      spaceForTabs.setInfo( getFromStore( IEditorPreferenceNames.EDITOR_SPACES_FOR_TABS ) );
    }*/

  }

}
