package net.sf.eclipsefp.haskell.ui.internal.preferences;

import java.util.LinkedList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.preferences.ScopedPreferenceStore;


/**
 * <p>Debug preferences</p>
  *
  * @author JP Moresmau
 */
public class DebugPP extends PreferencePage implements IWorkbenchPreferencePage {

  public static void initializeDefaults( final IPreferenceStore store ) {
    store.setDefault( ICorePreferenceNames.DEBUG_BREAK_ON_ERROR, false );
    store.setDefault( ICorePreferenceNames.DEBUG_BREAK_ON_EXCEPTION, false );
    store.setDefault( ICorePreferenceNames.DEBUG_PRINT_WITH_SHOW, true );
    store.setDefault( ICorePreferenceNames.RUN_COMMAND_HISTORY_MAX, 20 );
  }

  private final List<Button> buttons=new LinkedList<>();
  private IntegerFieldEditor historyMax;

  public DebugPP() {
   super(UITexts.preferences_debug_title);
   setDescription(UITexts.preferences_debug_description );
  }

  @Override
  protected Control createContents( final Composite parent ) {
    initializeDialogUnits( parent );

    Composite result = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.marginHeight =
      convertVerticalDLUsToPixels( IDialogConstants.VERTICAL_MARGIN );
    layout.marginWidth = 0;
    layout.verticalSpacing = convertVerticalDLUsToPixels( 10 );
    layout.horizontalSpacing =
      convertHorizontalDLUsToPixels( IDialogConstants.HORIZONTAL_SPACING );
    layout.numColumns = 2;
    result.setLayout( layout );

    addButton(result,ICorePreferenceNames.DEBUG_PRINT_WITH_SHOW,UITexts.preferences_debug_print_with_show);
    addButton(result,ICorePreferenceNames.DEBUG_BREAK_ON_ERROR,UITexts.preferences_debug_break_on_error);
    addButton(result,ICorePreferenceNames.DEBUG_BREAK_ON_EXCEPTION,UITexts.preferences_debug_break_on_exception);

    historyMax=new IntegerFieldEditor(ICorePreferenceNames.RUN_COMMAND_HISTORY_MAX,  UITexts.preferences_run_command_history_max, result );
    historyMax.setPreferenceStore( getPreferenceStore() );
    historyMax.setValidRange( 1, 100 );
    historyMax.setPage( this );
    historyMax.setTextLimit( 3 );
    historyMax.fillIntoGrid( result, 2 );

    historyMax.load();
    Dialog.applyDialogFont( result );
    return result;
  }

  private void addButton(final Composite result,final String pref,final String text){
    Button b=new Button(result,SWT.CHECK);
    buttons.add(b);
    b.setText( text );
    b.setData( pref );
    b.setSelection( getPreferenceStore().getBoolean( pref ) );
    GridData gd=new GridData(GridData.GRAB_HORIZONTAL);
    gd.horizontalSpan=2;
    b.setLayoutData(gd);
  }

  @Override
  public void init( final IWorkbench workbench ) {
    // NOOP
  }

  @Override
  protected void performDefaults() {
    IPreferenceStore store = getPreferenceStore();
    for (Button b:buttons){
      String pref=(String)b.getData();
      b.setSelection( store.getDefaultBoolean( pref ) );
    }
    historyMax.loadDefault();
    super.performDefaults();
  }

  @Override
  public boolean performOk() {
    IPreferenceStore store = getPreferenceStore();
    for (Button b:buttons){
      String pref=(String)b.getData();
      store.setValue( pref, b.getSelection() );
    }
    historyMax.store();
    /*try {
      new InstanceScope().getNode(HaskellCorePlugin.getPluginId()).flush();
    } catch( BackingStoreException ex ) {
      HaskellCorePlugin.log( ex );
    }*/

    return super.performOk();
  }

  @Override
  protected IPreferenceStore doGetPreferenceStore() {
    return new ScopedPreferenceStore(InstanceScope.INSTANCE,
               HaskellCorePlugin.getPluginId());
  }

}
