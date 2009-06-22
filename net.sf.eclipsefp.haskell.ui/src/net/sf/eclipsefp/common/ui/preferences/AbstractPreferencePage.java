package net.sf.eclipsefp.common.ui.preferences;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;

import org.eclipse.core.runtime.preferences.InstanceScope;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.osgi.service.prefs.BackingStoreException;

public abstract class AbstractPreferencePage extends PreferencePage
implements IWorkbenchPreferencePage {

	/**
	 * List of all preference controls managed by this object.
	 */
	private final List<PreferenceControl> controls = new ArrayList<PreferenceControl>();

	public AbstractPreferencePage() {
		super();
	}

	public AbstractPreferencePage(final String title) {
		super(title);
	}

	public AbstractPreferencePage(final String title, final ImageDescriptor image) {
		super(title, image);
	}

	public void init(final IWorkbench workbench) {
		// do nothing; subclasses may override
	}

	/**
	 * Loads the values of all {@link PreferenceControl}s on this page.
	 */
	public void loadControlValues() {
		IPreferenceStore prefStore = getPreferenceStore();
		for (PreferenceControl control : controls) {
			control.load(prefStore);
		}
	}

	/**
	 * Stores preferences from the controls created by the createXxx methods
	 * into the preference store returned by {@link #getPreferenceStore},
	 * then saves the preference store.
	 *
	 * @return true if saving succeeded
	 */
	@Override
	public boolean performOk() {
		IPreferenceStore prefStore = getPreferenceStore();
		for (PreferenceControl control : controls) {
			control.store(prefStore);
		}
		try {
			new InstanceScope().getNode(HaskellUIPlugin.getPluginId()).flush();
		} catch (BackingStoreException ex) {
			return false;
		}
		return true;
	}

	/**
	 * Loads the default values into the controls.
	 */
	@Override
	public void performDefaults() {
		IPreferenceStore prefStore = getPreferenceStore();
		for (PreferenceControl control : controls) {
			control.loadDefault(prefStore);
		}
	}

	protected Label createLabel(final Composite parent, final String text) {
		Label label = new Label(parent, SWT.LEFT);
		label.setText(text);
		return label;
	}

	protected CheckboxPreferenceControl createCheckboxControl(final Composite parent, final String text, final String name) {
		CheckboxPreferenceControl control = new CheckboxPreferenceControl(parent, text, name);
		controls.add(control);
		return control;
	}

	protected StringPreferenceControl createStringControl(final Composite parent, final String label, final String name, final int maxLength) {
		StringPreferenceControl control = new StringPreferenceControl(parent, label, name, maxLength);
		controls.add(control);
		return control;
	}

	protected IntegerPreferenceControl createIntegerControl(final Composite parent, final String label, final String name, final int maxLength) {
		IntegerPreferenceControl control = new IntegerPreferenceControl(parent, label, name, maxLength);
		controls.add(control);
		return control;
	}

}