package net.sf.eclipsefp.haskell.core.halamo;

import java.util.Hashtable;
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
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;

/**
 * The model manager takes care of all language models on the workspace.
 * 
 * @author Thiago Arrais
 */
public class HaskellModelManager implements IHaskellModelManager {

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

	/**
	 * <p>
	 * returns the compilation unit associated with the passed file resource.
	 * </p>
	 */
	public ICompilationUnit getCompilationUnit(final IFile file) {
		Assert.isNotNull(file);
		Assert.isTrue(file.exists());
		//TODO user parser proxy here
		try {
			return ParserManager.getInstance().getParser().parse(file);
		} catch (CoreException e) {
			return NullCompilationUnit.getInstance();
		}
	}

	private void initializeResourceChangeMonitor() {
		WorkspaceChangeMonitor changeMonitor = new WorkspaceChangeMonitor();
		changeMonitor.observeChangesOn(getWorkspace());
	}

	private IHaskellModel buildModelFor(final IProject project)
		throws CoreException
	{
		IHaskellModel prjModel = getModelFor(project);
		IHaskellProject hsProject = HaskellProjectManager.get(project);
		//TODO parametrize the sourcefolder
		IContainer sources = hsProject.getSourceFolder();
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

	private IWorkspace getWorkspace() {
		return fWorkspace;
	}

}
