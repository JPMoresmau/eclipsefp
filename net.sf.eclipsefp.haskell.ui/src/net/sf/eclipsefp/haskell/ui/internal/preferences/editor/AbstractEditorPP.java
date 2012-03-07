package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.common.ui.dialog.BooleanDialogField;
import net.sf.eclipsefp.common.ui.dialog.DialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.common.ui.preferences.overlay.OverlayPreferenceStore;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.editors.text.TextEditorPreferenceConstants;
import org.osgi.service.prefs.BackingStoreException;

/**
 * <p>Abstract super class for all editors preference pages (replace Tab and Editor Tab)</p>
  *
  * @author JP Moresmau
 */
public abstract class AbstractEditorPP extends PreferencePage implements IWorkbenchPreferencePage,
  IEditorPreferenceNames {

  public static void initializeDefaultValues( final IPreferenceStore store ) {
    TextEditorPreferenceConstants.initializeDefaultValues( store );
    DefaultEditorPreferenceInitializer.initializeDefaultValues( store );
  }

  private OverlayPreferenceStore overlayStore;
  protected Tab tab;

  @Override
  public void dispose() {
    if( overlayStore != null ) {
      overlayStore.stopListening();
      overlayStore = null;
    }
    super.dispose();
  }

  @Override
  public boolean performOk() {
    overlayStore.propagate();
    try {
      InstanceScope.INSTANCE.getNode(HaskellUIPlugin.getPluginId()).flush();
    } catch( BackingStoreException ex ) {
      HaskellUIPlugin.log( ex );
    }
    return true;
  }


  @Override
  protected void performDefaults() {
    overlayStore.loadDefaults();
    super.performDefaults();
  }

  // interface methods of IWorkbenchPreferencePage
  ////////////////////////////////////////////////

  public void init( final IWorkbench workbench ) {
    //setPreferenceStore( HaskellUIPlugin.getDefault() .getPreferenceStore() );
    overlayStore = createOverlayStore();
    overlayStore.load();
    overlayStore.startListening();
    setPreferenceStore(overlayStore);

    tab=new Tab(getPreferenceStore()){
      @Override
      public Control createControl( final Composite parent ) {
        return null;
      }
      public void propertyChange( final PropertyChangeEvent arg0 ) {
        // NOOP
      }
    };

  }

  private OverlayPreferenceStore createOverlayStore() {
    //IPreferenceStore prefStore = getPreferenceStore();
    OverlayPreferenceStore store = new OverlayPreferenceStore( HaskellUIPlugin.getDefault() .getPreferenceStore() );

    addPreferences( store );

    return store;
  }


  // functionality for subclasses
  ///////////////////////////////

  DialogField createBooleanField( final Composite parent,
                                  final String text,
                                  final String name ) {
    BooleanDialogField result = new BooleanDialogField( parent, text );
    result.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        boolean selected = ( ( Boolean )newInfo ).booleanValue();
        getPreferenceStore().setValue( name, selected );
      }
    } );
    result.setInfo( getFromStore( name ) );
    return result;
  }


  // helping methods
  //////////////////

  Boolean getFromStore( final String name ) {
    boolean value = getPreferenceStore().getBoolean( name );
    return ( value ) ? Boolean.TRUE : Boolean.FALSE;
  }


  protected abstract void addPreferences(final OverlayPreferenceStore store);
}
