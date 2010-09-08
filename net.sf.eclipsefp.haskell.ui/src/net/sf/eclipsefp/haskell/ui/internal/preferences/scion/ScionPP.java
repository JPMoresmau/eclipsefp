package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import java.io.File;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.internal.util.UIUtils;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * The Scion preferences page in the Preferences dialog.
 *
 * @author Thomas ten Cate
 */

public class ScionPP
	extends PreferencePage
	implements IWorkbenchPreferencePage, IPreferenceConstants {

  public static final String PAGE_ID = ScionPP.class.getName();

	private ExecutableFileFieldEditor serverExecutableField;
	private BooleanFieldEditor serverBuiltInField;
	private ButtonFieldEditor autodetect;
	private CabalImplsBlock cabalBlock;
	private Composite prefComp;

	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	@Override
  protected Control createContents( final Composite parent ) {
	  initializeDialogUnits( parent );
	  noDefaultAndApplyButton();

	  prefComp = new Composite(parent, SWT.NONE);

	  GridLayout glayout = new GridLayout(1, true);
	  glayout.marginHeight = 0;
	  glayout.marginWidth = 0;
	  prefComp.setLayout( glayout );

	  UIUtils.createMessageLabel( prefComp, UITexts.scion_preferences_title, 1, SWT.DEFAULT );
	  UIUtils.createLineSpacer( prefComp, 1 );

    cabalBlock = new CabalImplsBlock();
    Control control = cabalBlock.createControl( prefComp );

    // GridData gdata = new GridData();
    // gdata.horizontalAlignment = SWT.FILL;
    // gdata.verticalAlignment = SWT.FILL;
    // control.setLayoutData( gdata );
    // IDialogSettings dlgSettings = HaskellUIPlugin.getDefault().getDialogSettings();
    // cabalBlock.restoreColumnSettings( dlgSettings, PAGE_ID );
    // UIUtils.createLineSpacer( prefComp, 1 );

    IPreferenceStore prefStore = getPreferenceStore();

    UIUtils.createMessageLabel (prefComp, UITexts.scion_preferences_title, 1, SWT.DEFAULT);

		serverBuiltInField = new BooleanFieldEditor( IPreferenceConstants.SCION_SERVER_BUILTIN,
		                                             UITexts.scionServerBuiltIn_label,
		                                             prefComp );
		serverBuiltInField.setPage( this );
		serverBuiltInField.setPreferenceStore( prefStore );
		serverBuiltInField.setPropertyChangeListener( new IPropertyChangeListener() {
      public void propertyChange( final PropertyChangeEvent event ) {
        Boolean b=(Boolean)event.getNewValue();
        autodetect.setEnabled( !b.booleanValue(), prefComp );
        serverExecutableField.setEnabled(  !b.booleanValue(), prefComp );
      }
    });
    serverBuiltInField.load();

		serverExecutableField = new ExecutableFileFieldEditor(IPreferenceConstants.SCION_SERVER_EXECUTABLE,
				NLS.bind(UITexts.scionServerExecutable_label, getServerExecutableName()),
				false, StringFieldEditor.VALIDATE_ON_KEY_STROKE, prefComp);
		serverExecutableField.setEmptyStringAllowed(true);
		serverExecutableField.setPage( this );
		serverExecutableField.setPreferenceStore( prefStore );
		serverExecutableField.load();

		autodetect = new ButtonFieldEditor(
				String.format(UITexts.autodetectButton_label, getServerExecutableName()),
				UITexts.autodetectButton_text,
				new SelectionAdapter() {
					@Override
					public void widgetSelected(final SelectionEvent e) {
						doDetectServer();
					}
				},
				prefComp);
		autodetect.setPage( this );
		autodetect.setPreferenceStore( prefStore );
		autodetect.load();

		return prefComp;
	}

	private void doDetectServer() {
		String server = detectScionServer();
		if (server == null) {
			MessageDialog.openError(getShell(),
					UITexts.autodetectButton_errorTitle,
					NLS.bind(UITexts.autodetectButton_errorMessage, getServerExecutableName()));
		} else {
			serverExecutableField.setStringValue(server);
		}
	}

	/**
	 * Attempts to autodetect the path to the Scion server executable.
	 *
	 * @return the filename of the Scion server, or null if it could not be found
	 */
	private String detectScionServer() {
	  File f=FileUtil.findExecutableInPath( getServerExecutableName() );
	  return f!=null?f.getAbsolutePath():null;
	}

	public static String getServerExecutableName() {
		return FileUtil.makeExecutableName("scion-server"); //$NON-NLS-1$
	}

	public static void initializeDefaults(final IPreferenceStore store) {
	  // scion might be on the path...
	  //store.setDefault(SCION_SERVER_EXECUTABLE, getServerExecutableName());
	  store.setDefault( SCION_SERVER_BUILTIN, true );
	}

  @Override
  public boolean performOk() {
/*
    IEclipsePreferences node = new InstanceScope().getNode( HaskellCorePlugin.getPluginId() );

    node.put( ICorePreferenceNames.HS_IMPLEMENTATIONS, implementationsBlock.getPref() );
    IHsImplementation impl = implementationsBlock.getCheckedHsImplementation();
    String name = ""; //$NON-NLS-1$
    if( impl != null ) {
      name = impl.getName();
    }
    node.put( ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION, name );

    try {
      node.flush();
    } catch( BackingStoreException ex ) {
      HaskellUIPlugin.log( ex );
    }

    IDialogSettings settings = HaskellUIPlugin.getDefault().getDialogSettings();
    implementationsBlock.saveColumnSettings( settings, DIALOG_SETTINGS_ID );
*/
    return super.performOk();
  }

  @Override
  public boolean performCancel() {
    return super.performCancel();
  }

  @Override
  public boolean isValid() {
    return super.isValid();
  }

	@Override
	public void dispose() {
	  // unused
	}

  public void init( final IWorkbench workbench ) {
    // unused
  }
}
