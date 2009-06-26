package net.sf.eclipsefp.haskell.scion.client.preferences;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * Class used to initialize default preference values. Referenced from plugin.xml.
 * 
 * @author Thomas ten Cate
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer implements IPreferenceConstants {

	/**
	 * Initializes the preferences for the Scion plugin.
	 * 
	 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
	 */
	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore store = ScionPlugin.getDefault().getPreferenceStore();
		store.setDefault(SCION_SERVER_EXECUTABLE,
				"");
	}

}
