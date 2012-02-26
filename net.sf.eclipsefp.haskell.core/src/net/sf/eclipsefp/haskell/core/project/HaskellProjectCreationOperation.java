package net.sf.eclipsefp.haskell.core.project;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.DescriptorFileInfo;
import net.sf.eclipsefp.haskell.core.internal.project.ProjectCreationOperation;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;

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
		//IPreferencesService service = Platform.getPreferencesService();
		//String sourcePath = service.getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.FOLDERS_SRC, null, null );
		//String outputPath = service.getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.FOLDERS_OUT, null, null );
		return new String[] { getSourceDir() }; // , outputPath
	}

	public static String getSourceDir(){
	  String sourcePath=null;
	  if (createFolders() ) {
	    IPreferencesService service = Platform.getPreferencesService();
	    sourcePath = service.getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.FOLDERS_SRC, null, null );
	  }
	  return sourcePath;
	}

	@Override
	protected DescriptorFileInfo getDescFileInfo() {
		return null;
//		new DescriptorFileInfo(
//				       HaskellProjectManager.HASKELL_PROJECT_DESCRIPTOR,
//				       createDescriptorContent());
	}

//	private String createDescriptorContent() {
//		if (! createFolders() ) {
//			return ""; //$NON-NLS-1$
//		}
//		IPreferencesService service = Platform.getPreferencesService();
//		return HaskellProjectManager.createDescriptorContent(
//		    service.getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.FOLDERS_SRC, null, null ),
//		   // service.getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.FOLDERS_OUT, null, null ),
//		    //service.getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.TARGET_BINARY, null, null ),
//		    service.getString( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.SELECTED_COMPILER, null, null ));
//	}

	public static boolean createFolders() {
	  return Platform.getPreferencesService().getBoolean( HaskellCorePlugin.getPluginId(), ICorePreferenceNames.FOLDERS_IN_NEW_PROJECT, false, null );
	}

}
