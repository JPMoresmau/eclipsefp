package net.sf.eclipsefp.haskell.scion.client.preferences;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
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
	public void createFieldEditors() {
		addField(new ExecutableFileFieldEditor(IPreferenceConstants.SCION_SERVER_EXECUTABLE, 
				"&Scion server executable:", true, StringFieldEditor.VALIDATE_ON_KEY_STROKE, getFieldEditorParent()));
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}
	
}