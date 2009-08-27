package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.FileUtil;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
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

  public static final String PAGE_ID = "net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ScionPP";

	private ExecutableFileFieldEditor serverExecutableField;

	public ScionPP() {
		super(GRID);
		setPreferenceStore(HaskellUIPlugin.getDefault().getPreferenceStore());
		setDescription("Preferences related to Scion, the Haskell IDE library");
	}

	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	@Override
	public void createFieldEditors() {
		Composite parent = getFieldEditorParent();

		serverExecutableField = new ExecutableFileFieldEditor(IPreferenceConstants.SCION_SERVER_EXECUTABLE,
				NLS.bind(UITexts.scionServerExecutable_label, getServerExecutableName()),
				true, StringFieldEditor.VALIDATE_ON_KEY_STROKE, parent);
		serverExecutableField.setEmptyStringAllowed(true);
		addField(serverExecutableField);

		ButtonFieldEditor autodetect = new ButtonFieldEditor(
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
		// build up a list of directories that might contain the scion-server binary
		List<File> candidates = new ArrayList<File>(32);

		// add all directories from the $PATH variable
		// TODO TtC this is Unix-only; Windows splits on semicolons I believe, and might do quoting/escaping?
		String path = System.getenv("PATH");
		for (String dir : path.split(":")) {
			candidates.add(new File(dir));
		}

		// add common bin directories from the user's home dir
		String[] homes = new String[] {
			System.getenv("HOME"),
			System.getProperty("user.home")
		};
		String[] userBins = new String[] {
			".cabal/bin",
			"usr/bin",
			"bin",
		};
		for (String home : homes) {
			for (String userBin : userBins) {
				candidates.add(new File(home, userBin));
			}
		}

		// add the current working directory
		String pwd = System.getProperty("user.dir");
		candidates.add(new File(pwd));

		String executableName = getServerExecutableName();

		for (File candidate : candidates) {
			File file = new File(candidate, executableName);
			if (file.isFile() && FileUtil.isExecutable(file)) {
				return file.getAbsolutePath();
			}
		}

		return null;
	}

	public static String getServerExecutableName() {
		return FileUtil.makeExecutableName("scion-server");
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(final IWorkbench workbench) {
	  // do nothing
	}

	public static void initializeDefaults(final IPreferenceStore store) {
	  // scion might be on the path...
	  store.setDefault(SCION_SERVER_EXECUTABLE, getServerExecutableName());
	}

}
