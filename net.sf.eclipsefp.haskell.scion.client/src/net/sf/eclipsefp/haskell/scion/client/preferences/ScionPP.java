package net.sf.eclipsefp.haskell.scion.client.preferences;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.util.FileUtil;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
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
	implements IWorkbenchPreferencePage {
	
	private ExecutableFileFieldEditor serverExecutableField;

	public ScionPP() {
		super(GRID);
		setPreferenceStore(ScionPlugin.getDefault().getPreferenceStore());
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
				String.format(ScionPlugin.getStringResource("ScionServerExecutable.label"), getServerExecutableName()),
				true, StringFieldEditor.VALIDATE_ON_KEY_STROKE, parent);
		serverExecutableField.setEmptyStringAllowed(true);
		addField(serverExecutableField);
		
		ButtonFieldEditor autodetect = new ButtonFieldEditor(
				String.format(ScionPlugin.getStringResource("AutodetectButton.label"), getServerExecutableName()),
				ScionPlugin.getStringResource("AutodetectButton.text"),
				new SelectionAdapter() {
					@Override
					public void widgetSelected(SelectionEvent e) {
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
					ScionPlugin.getStringResource("AutodetectButton.errorTitle"),
					String.format(ScionPlugin.getStringResource("AutodetectButton.errorMessage"), getServerExecutableName()));
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
		// build up a list of directories that might contain the scion_server binary
		List<File> candidates = new ArrayList<File>(32);
		
		// add all directories from the $PATH variable
		// TODO this is Unix-only; Windows splits on semicolons I believe, and might do quoting/escaping?
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
		
		// TODO Windows wants .exe
		String executableName = getServerExecutableName();
		
		for (File candidate : candidates) {
			File file = new File(candidate, executableName);
			if (file.isFile() && FileUtil.isExecutable(file)) {
				return file.getAbsolutePath();
			}
		}
		
		return null;
	}
	
	private String getServerExecutableName() {
		// TODO Windows
		return "scion_server";
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}
	
}
