package net.sf.eclipsefp.haskell.ui.wizards;

import org.eclipse.jface.preference.IPreferenceStore;

import net.sf.eclipsefp.common.ui.wizards.DescriptorFileInfo;
import net.sf.eclipsefp.common.ui.wizards.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.preferences.IPreferenceConstants;

public class HaskellProjectCreationOperation extends ProjectCreationOperation {

	private IPreferenceStore fStore;

	public HaskellProjectCreationOperation() {
		this(HaskellUIPlugin.getDefault().getPreferenceStore());
	}

	public HaskellProjectCreationOperation(IPreferenceStore store) {
		fStore = store;
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
		String sourcePath = getPreference(IPreferenceConstants.FOLDERS_SRC);
		String outputPath = getPreference(IPreferenceConstants.FOLDERS_OUT);
		String binPath = getPreference(IPreferenceConstants.FOLDERS_BIN);
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
			return "";
		}
		return HaskellProjectManager.createDescriptorContent(
			       getPreference(IPreferenceConstants.FOLDERS_SRC),
			       getPreference(IPreferenceConstants.FOLDERS_OUT),
			       getPreference(IPreferenceConstants.FOLDERS_BIN),
			       getPreference(IPreferenceConstants.TARGET_BINARY));
	}

	private boolean createFolders() {
	    final String prefKey = IPreferenceConstants.FOLDERS_IN_NEW_PROJECT;
		return getStore().getBoolean(prefKey);
	}

	private String getPreference(final String name) {
		return getStore().getString(name);
	}
	
	private IPreferenceStore getStore() {
		return fStore;
	}
}
