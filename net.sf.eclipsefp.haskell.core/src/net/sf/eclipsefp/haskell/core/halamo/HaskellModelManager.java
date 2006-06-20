package net.sf.eclipsefp.haskell.core.halamo;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import net.sf.eclipsefp.common.core.util.Assert;
import net.sf.eclipsefp.haskell.core.parser.IHaskellParser;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

/**
 * The model manager takes care of all language models on the workspace.
 * 
 * @author Thiago Arrais
 */
public class HaskellModelManager implements IHaskellModelManager {

	private Map<String, ProjectModel> htProjectModels = new Hashtable<String, ProjectModel>();

	private Map<IProject, HaskellLanguageModel> fLanguageModels = new Hashtable<IProject, HaskellLanguageModel>();

	private IWorkspace fWorkspace;
	
	public HaskellModelManager(IWorkspace workspace) {
		fWorkspace = workspace;
	}

	/**
	 * Gets the corresponding IHaskellModel object for a project.
	 * This method doesn't initialize the project (i.e. the resulting object
	 * does not contain any of the modules defined on the project), it is
	 * supposed to be used as a lazy proxy only.
	 * 
	 * @see buildModelFor
	 */
	public IHaskellModel getModelFor(IProject project) {
		HaskellLanguageModel result = fLanguageModels.get(project);
		if (null == result) {
			result = new HaskellLanguageModel();
			fLanguageModels.put(project, result);
		}
		return result;
	}
	
	/**
	 * reads all information from all Haskell projects into the Halamo.
	 * 
	 * Need to make this much more lazy (by proxy mechanism similar to the Java
	 * Model in JDT.
	 */
	public void initialize() throws CoreException {
		// start the monitoring for ws saving and resource changes
		WSSaveParticipant.initialize();
		initializeResourceChangeMonitor();

		IProject[] projects = getWorkspace().getRoot().getProjects();
		for (int i = 0; i < projects.length; i++) {
			if (projects[i].isOpen()
					&& projects[i].hasNature(HaskellNature.NATURE_ID)) {
				buildModelFor(projects[i]);
			}
		}
	}

	public IModule[] getAllModules(final IProject project) {
		List<IModule> alResult = new ArrayList<IModule>();

		IHaskellProject hsProject = HaskellProjectManager.get(project);
		IPath sourcePath = hsProject.getSourcePath();
		IFolder folder = project.getFolder(sourcePath);
		IResource[] ress = new IResource[0];
		try {
			ress = folder.members(false);
		} catch (CoreException ex) {
			// TODO Auto-generated catch block
			ex.printStackTrace();
		}
		for (int i = 0; i < ress.length; i++) {
			if (ress[i] instanceof IFile) {
				IFile file = (IFile) ress[i];
				ICompilationUnit cu = getCompilationUnit(file);
				IModule[] modules = cu.getModules();
				for (int j = 0; j < modules.length; j++) {
					alResult.add(modules[j]);
				}
			}
		}

		// TODO qualified
		IModule[] result = new IModule[alResult.size()];
		alResult.toArray(result);
		return result;
	}

	/**
	 * <p>
	 * returns the compilation unit associated with the passed file resource.
	 * </p>
	 */
	public ICompilationUnit getCompilationUnit(final IFile file) {
		Assert.isNotNull(file);
		Assert.isTrue(file.exists());
		return getProjectModel(file).getCompilationUnit(file);
	}

	/**
	 * <p>
	 * returns all compilation units that depend on the passed compilation unit,
	 * that is, all Haskell source files that comtain modules which import any
	 * modul in the passed compilation unit.
	 * </p>
	 */
	public ICompilationUnit[] getDependentCUs(final ICompilationUnit cu) {
		return getProjectModel(cu).getDependentCUs(cu);
	}

	/**
	 * invalidates a file, meaning that the corresponding compilation unit has
	 * to be parsed anew if it is requested again.
	 */
	public void invalidate(final IFile file) {
		getProjectModel(file).invalidate(file);
	}

	public ICompilationUnit getByName(final String name, final IProject project) {
		return getProjectModel(project).getByName(name);
	}

	private void initializeResourceChangeMonitor() {
		WorkspaceChangeMonitor changeMonitor = new WorkspaceChangeMonitor();
		changeMonitor.observeChangesOn(getWorkspace());
	}

	private IHaskellModel buildModelFor(final IProject project)
		throws CoreException
	{
		IHaskellModel prjModel = getModelFor(project);
		//TODO parametrize the sourcefolder
		IContainer sources = project.getFolder("src");
		for(IResource resource : sources.members()) {
			if( resource.getType() == IResource.FILE ) {
				IFile file = (IFile) resource;
				if( ResourceUtil.hasHaskellExtension( file ) ) {
					IHaskellParser parser = ParserManager.getInstance()
	                            				.getParser();
					try {
						ICompilationUnit unit = parser.parse(file);
						prjModel.putModule(unit.getModules()[0]);
					} catch (CoreException e) {
						//ignore parsing exception and go on
					}
				}
			}
		}
		return prjModel;
	}

	private ProjectModel getProjectModel(final ICompilationUnit cu) {
		IFile file = cu.getUnderlyingResource();
		Assert.isNotNull(file);
		return getProjectModel(file);
	}

	private ProjectModel getProjectModel(final IFile file) {
		return getProjectModel(file.getProject());
	}

	private ProjectModel getProjectModel(final IProject project) {
		String key = getKey(project);
		ProjectModel result = htProjectModels.get(key);
		if (result == null) {
			result = new ProjectModel(project);
			htProjectModels.put(key, result);
		}
		return result;
	}

	private String getKey(final IProject project) {
		return project.getName();
	}

	private IWorkspace getWorkspace() {
		return fWorkspace;
	}

}
