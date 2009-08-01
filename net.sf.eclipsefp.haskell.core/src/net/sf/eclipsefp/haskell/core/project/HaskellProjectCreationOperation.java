package net.sf.eclipsefp.haskell.core.project;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.ExecutableBuildTarget;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;

public class HaskellProjectCreationOperation extends ProjectCreationOperation {

	public HaskellProjectCreationOperation() {
	  super();
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
		return new String[] { getDefaultSourcePath(), getDefaultBuildPath(), getDefaultOutputPath() };
	}

	private boolean createFolders() {
	  return Platform.getPreferencesService().getBoolean( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.FOLDERS_IN_NEW_PROJECT, false, null );
	}

	private String getDefaultSourcePath() {
	  return Platform.getPreferencesService().getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.FOLDERS_SRC, null, null );
	}

	private String getDefaultOutputPath() {
	  return Platform.getPreferencesService().getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.FOLDERS_OUT, null, null );
	}

  private String getDefaultBuildPath() {
    return Platform.getPreferencesService().getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.FOLDERS_BUILD, null, null );
  }

  private String getDefaultTarget() {
    return Platform.getPreferencesService().getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.TARGET_BINARY, null, null );
  }

  @Override
  protected void createNatureProject( final IProgressMonitor monitor, final IProject project ) {
    IHaskellProject hsProject = HaskellProjectManager.get( project );
    hsProject.addSourcePath( Path.fromPortableString( getDefaultSourcePath() ) );
    hsProject.setOutputPath( Path.fromPortableString( getDefaultOutputPath() ) );
    hsProject.setBuildPath( Path.fromPortableString( getDefaultBuildPath() ) );
    hsProject.addTarget( new ExecutableBuildTarget( Path.fromPortableString( getDefaultTarget() ) ) );
    hsProject.saveDescriptor();
  }

}
