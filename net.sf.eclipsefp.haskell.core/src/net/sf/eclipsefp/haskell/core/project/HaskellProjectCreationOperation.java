package net.sf.eclipsefp.haskell.core.project;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.DescriptorFileInfo;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import org.eclipse.core.runtime.Preferences;

public class HaskellProjectCreationOperation extends ProjectCreationOperation {

	private final Preferences fPreferences;

	public HaskellProjectCreationOperation() {
    this( HaskellCorePlugin.getDefault().getPluginPreferences() );
  }

  public HaskellProjectCreationOperation( final Preferences prefs ) {
    fPreferences = prefs;
  }

	@Override
	protected String[] getProjectNatures() {
		return new String[] { HaskellNature.NATURE_ID };
	}

	@Override
	protected String[] getDirectories() {
		if (! createFolders() ) {
			return new String[0];
		}
		String sourcePath = getPreference(ICorePreferenceNames.FOLDERS_SRC);
		String outputPath = getPreference(ICorePreferenceNames.FOLDERS_OUT);
		String binPath = getPreference(ICorePreferenceNames.FOLDERS_BIN);
		return new String[] { sourcePath, outputPath, binPath };
	}

	@Override
	protected DescriptorFileInfo getDescFileInfo() {
		return new DescriptorFileInfo(
				       HaskellProjectManager.HASKELL_PROJECT_DESCRIPTOR,
				       createDescriptorContent());
	}

	private String createDescriptorContent() {
		if (! createFolders() ) {
			return ""; //$NON-NLS-1$
		}
		return HaskellProjectManager.createDescriptorContent(
			       getPreference(ICorePreferenceNames.FOLDERS_SRC),
			       getPreference(ICorePreferenceNames.FOLDERS_OUT),
			       getPreference(ICorePreferenceNames.FOLDERS_BIN),
			       getPreference(ICorePreferenceNames.TARGET_BINARY),
			       getPreference(ICorePreferenceNames.SELECTED_COMPILER));
	}

	private boolean createFolders() {
	  String prefKey = ICorePreferenceNames.FOLDERS_IN_NEW_PROJECT;
		return getStore().getBoolean(prefKey);
	}

	private String getPreference(final String name) {
		return getStore().getString(name);
	}

	private Preferences getStore() {
		return fPreferences;
	}
}
