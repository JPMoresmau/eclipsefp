// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;

/**
 * <p>
 * watches all resource changes in the workspace and informs the language model
 * about them.
 * </p>
 * 
 * <p>
 * Singleton
 * </p>
 * 
 * @author Leif Frenzel
 */
public class ResourceChangeMonitor implements IResourceChangeListener {

	/** the resource change types the ResourceChangeMonitor is interested in. */
	public static final int TYPES = IResourceChangeEvent.PRE_BUILD
			| IResourceChangeEvent.POST_BUILD
			| IResourceChangeEvent.POST_CHANGE
			| IResourceChangeEvent.PRE_DELETE | IResourceChangeEvent.PRE_CLOSE;

	private IResourceDeltaVisitor visitor = new ResourceDeltaVisitor();

	private IHaskellModel fLanguageModel;

	public ResourceChangeMonitor(IProject project) {
		this(HaskellModelManager.getInstance().getModelFor(project));
	}
	
	public ResourceChangeMonitor(IHaskellModel model) {
		fLanguageModel = model;
	}

	public void resourceChanged(final IResourceChangeEvent event) {
		if (event.getType() == IResourceChangeEvent.POST_CHANGE) { // TODO ??
			IResourceDelta[] projectDeltas = getProjectDeltas(event.getDelta());
			for (int i = 0; i < projectDeltas.length; i++) {
				process(projectDeltas[i]);
			}
		}
	}

	// helping methods
	// ////////////////

	private void process(final IResourceDelta projectDelta) {
		IProject project = (IProject) projectDelta.getResource();
		try {
			if (project.exists() && project.isOpen()
					&& project.hasNature(HaskellNature.NATURE_ID)) {
				IResourceDelta sourceDelta = getSourceDelta(projectDelta);
				if (sourceDelta != null) {
					sourceDelta.accept(visitor);
				}
			}
		} catch (CoreException cex) {
			String msg = "Could not process resource changes in the Haskell "
					+ "language model.\n" + "Project: " + project.getName();
			HaskellCorePlugin.log(msg, cex);
		}
	}

	private IResourceDelta getSourceDelta(final IResourceDelta projectDelta) {
		IProject project = (IProject) projectDelta.getResource();
		IHaskellProject hp = HaskellProjectManager.get(project);
		return projectDelta.findMember(hp.getSourcePath());
	}

	private IResourceDelta[] getProjectDeltas(final IResourceDelta rootDelta) {
		return rootDelta.getAffectedChildren(TYPES, IResource.PROJECT);
	}

	// inner classes
	// //////////////

	/**
	 * visits the resource delta and invalidates in the model what has changed
	 * in the underlying resource.
	 */
	private final class ResourceDeltaVisitor implements IResourceDeltaVisitor {
		public boolean visit(final IResourceDelta delta) throws CoreException {
			// only interested in changed resources (not added or removed)
			IResource resource = delta.getResource();
			// only interested in Haskell source files
			if (resource.getType() == IResource.FILE
					&& ResourceUtil.hasHaskellExtension(resource))
			{
				System.out.println("Processing change for " + resource
						+ " kind: " + delta.getKind());
				if ((delta.getKind() & (IResourceDelta.ADDED | IResourceDelta.CHANGED)) != 0) {
					//TODO clean this mess
					ICompilationUnit unit = ParserManager.getInstance().getParser().parse((IFile) resource);
					getLanguageModel().putModule(unit.getModules()[0]);
				}
			}
			return true;
		}
	}

	public IHaskellModel getLanguageModel() {
		return fLanguageModel;
	}
}