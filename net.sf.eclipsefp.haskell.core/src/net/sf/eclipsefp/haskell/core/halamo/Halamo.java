// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import net.sf.eclipsefp.common.core.util.Assert;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;

/**
 * <p>
 * The singleton entry point for the Haskell Language Model. A model contains
 * information about the modules in a project, their exported functions, imports
 * and exports, and dependencies.
 * </p>
 * 
 * <p>
 * Singleton
 * </p>
 * 
 * @author Leif Frenzel
 */
public class Halamo implements IHaskellModel {

	/** the singleton instance of the Haskell Language Model. */
	private static Halamo _instance;

	private static boolean tracing = HaskellCorePlugin.isTracing("halamo");

	private Map<String, ProjectModel> htProjectModels = new Hashtable<String, ProjectModel>();

	private Map<String, IModule> fModules = new Hashtable<String, IModule>();

	/**
	 * constructs the singleton instance of the Haskell Language Model. Private
	 * in order to ensure the singleton pattern.
	 */
	protected Halamo() {
		//no need to do anything
	}

	/**
	 * <p>
	 * returns a reference to the singleton instance of the Haskell Language
	 * Model.
	 * </p>
	 */
	public static synchronized Halamo getInstance() {
		if (_instance == null) {
			_instance = new Halamo();
		}
		return _instance;
	}

	/**
	 * reads all information from all Haskell projects into the Halamo.
	 * 
	 * Need to make this much more lazy (by proxy mechanism similar to the Java
	 * Model in JDT.
	 */
	public void initialize() throws CoreException {
		long start = System.currentTimeMillis();

		// start the monitoring for ws saving and resource changes
		WSSaveParticipant.initialize();
		initializeResourceChangeMonitor();

		IProject[] projects = getWorkspace().getRoot().getProjects();
		for (int i = 0; i < projects.length; i++) {
			if (projects[i].isOpen()
					&& projects[i].hasNature(HaskellNature.NATURE_ID)) {
				initialize(projects[i]);
			}
		}
		if (tracing) {
			long time = System.currentTimeMillis() - start;
			System.out.println("Initializing Halamo ... " + time + " ms.");
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

	// helping methods
	// ////////////////

	private ProjectModel getProjectModel(final IFile file) {
		return getProjectModel(file.getProject());
	}

	private ProjectModel getProjectModel(final ICompilationUnit cu) {
		IFile file = cu.getUnderlyingResource();
		Assert.isNotNull(file);
		return getProjectModel(file);
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

	private void initialize(final IProject project) throws CoreException {
		ProjectModel model = getProjectModel(project);
		model.initialize();
	}

	private void initializeResourceChangeMonitor() {
		ResourceChangeMonitor resChaMon = new ResourceChangeMonitor();
		getWorkspace().addResourceChangeListener(resChaMon,
				ResourceChangeMonitor.TYPES);
	}

	private IWorkspace getWorkspace() {
		return HaskellCorePlugin.getDefault().getWorkspace();
	}

	public Scope getScopeFor(IModule module) {
		return new ScopeCalculator().computeScopeFor(module);
	}

	public void putModule(IModule module) {
		fModules.put(module.getName(), module);
	}

	public IModule getModule(String name) {
		return fModules.get(name);
	}
}