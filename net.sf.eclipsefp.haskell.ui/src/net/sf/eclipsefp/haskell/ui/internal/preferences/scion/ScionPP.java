package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import java.io.File;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * The Scion preferences page in the Preferences dialog.
 *
 * @author Thomas ten Cate
 */

public class ScionPP
	extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage, IPreferenceConstants {

  public static final String PAGE_ID = "net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ScionPP"; //$NON-NLS-1$

	private ExecutableFileFieldEditor serverExecutableField;
	private BooleanFieldEditor serverBuiltInField;
	private ButtonFieldEditor autodetect;
	private Composite parent;

	public ScionPP() {
		super(GRID);
		setPreferenceStore(HaskellUIPlugin.getDefault().getPreferenceStore());
		setDescription(UITexts.scion_preferences_title);
	}

	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	@Override
	public void createFieldEditors() {
		parent = getFieldEditorParent();

		serverBuiltInField=new BooleanFieldEditor( IPreferenceConstants.SCION_SERVER_BUILTIN, UITexts.scionServerBuiltIn_label, parent );
		addField(serverBuiltInField);

		serverExecutableField = new ExecutableFileFieldEditor(IPreferenceConstants.SCION_SERVER_EXECUTABLE,
				NLS.bind(UITexts.scionServerExecutable_label, getServerExecutableName()),
				false, StringFieldEditor.VALIDATE_ON_KEY_STROKE, parent);
		serverExecutableField.setEmptyStringAllowed(true);
		addField(serverExecutableField);

		autodetect = new ButtonFieldEditor(
				String.format(UITexts.autodetectButton_label, getServerExecutableName()),
				UITexts.autodetectButton_text,
				new SelectionAdapter() {
					@Override
					public void widgetSelected(final SelectionEvent e) {
						doDetectServer();
					}
				},
				parent);
		addField(autodetect);



	}

	@Override
	public void propertyChange( final PropertyChangeEvent event ) {
	  if (event.getSource()==serverBuiltInField ){
	    Boolean b=(Boolean)event.getNewValue();
      autodetect.setEnabled( !b.booleanValue(), parent );
      serverExecutableField.setEnabled(  !b.booleanValue(), parent );
	  }
	  super.propertyChange( event );
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
	  File f=FileUtil.findExecutableInPath( getServerExecutableName());
	  return f!=null?f.getAbsolutePath():null;
	}

	public static String getServerExecutableName() {
		return FileUtil.makeExecutableName("scion-server"); //$NON-NLS-1$
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(final IWorkbench workbench) {
	  // do nothing
	}

	public static void initializeDefaults(final IPreferenceStore store) {
	  // scion might be on the path...
	  //store.setDefault(SCION_SERVER_EXECUTABLE, getServerExecutableName());
	  store.setDefault( SCION_SERVER_BUILTIN, true );
	}

	@Override
	protected void initialize() {
	  super.initialize();
	  autodetect.setEnabled( !serverBuiltInField.getBooleanValue(), parent );
	  serverExecutableField.setEnabled(  !serverBuiltInField.getBooleanValue(), parent );
	}

}
