// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.core.project.*;
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

	// interface methods of IResourceChangeListener
	// /////////////////////////////////////////////

	public ResourceChangeMonitor() {
		this(Halamo.getInstance());
	}
	
	public ResourceChangeMonitor(IHaskellModel languageModel) {
		fLanguageModel = languageModel;
	}

	public void resourceChanged(final IResourceChangeEvent event) {
		// long start = System.currentTimeMillis();
		if (event.getType() == IResourceChangeEvent.POST_CHANGE) { // TODO ??
			IResourceDelta[] projectDeltas = getProjectDeltas(event.getDelta());
			for (int i = 0; i < projectDeltas.length; i++) {
				process(projectDeltas[i]);
			}
		}
		// long time = System.currentTimeMillis() - start;
		// System.out.println( "Resource change op took " + time + "ms." );
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
				if ((delta.getKind() & IResourceDelta.ADDED) != 0) {
					//TODO clean this mess
					ICompilationUnit unit = ParserManager.getInstance().getParser().parse((IFile) resource);
					getLanguageModel().addModule(unit.getModules()[0]);
				}
			}
			return true;
		}
	}

	public IHaskellModel getLanguageModel() {
		return fLanguageModel;
	}
}