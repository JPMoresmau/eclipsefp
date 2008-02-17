package net.sf.eclipsefp.haskell.core.halamo;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.parser.IHaskellParser;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;

/**
 * Watches for changes on an specific project and updates the internal language
 * model when needed
 *
 * @see WorkspaceChangeMonitor
 *
 * @author Thiago Arrais
 */
public class ProjectChangeMonitor implements IResourceChangeListener {

	private final IResourceDeltaVisitor visitor = new ResourceDeltaVisitor();

	private final IHaskellModel fLanguageModel;

	public ProjectChangeMonitor(final IProject project) {
		this(HaskellCorePlugin.getDefaultModelManager().getModelFor(project));
	}

	public ProjectChangeMonitor(final IHaskellModel model) {
		fLanguageModel = model;
	}

	public void resourceChanged(final IResourceChangeEvent event) {
		if (event.getType() == IResourceChangeEvent.POST_CHANGE) {
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
			String msg = "Could not process resource changes in the Haskell " //$NON-NLS-1$
					+ "language model.\n" + "Project: " + project.getName();  //$NON-NLS-1$//$NON-NLS-2$
			HaskellCorePlugin.log(msg, cex);
		}
	}

	private IResourceDelta getSourceDelta(final IResourceDelta projectDelta) {
		IProject project = (IProject) projectDelta.getResource();
		IHaskellProject hp = HaskellProjectManager.get(project);
		return projectDelta.findMember(hp.getSourcePath());
	}

	private IResourceDelta[] getProjectDeltas(final IResourceDelta rootDelta) {
		return rootDelta.getAffectedChildren(WorkspaceChangeMonitor.TYPES, IResource.PROJECT);
	}

	// inner classes
	// //////////////

	/**
	 * visits the resource delta and invalidates in the model what has changed
	 * in the underlying resource.
	 */
	private final class ResourceDeltaVisitor implements IResourceDeltaVisitor {
		public boolean visit(final IResourceDelta delta) throws CoreException {
			IResource resource = delta.getResource();
			// only interested in Haskell source files
			if (!(  IResource.FILE == resource.getType()
				 && ResourceUtil.hasHaskellExtension(resource)))
			{
				return true;
			}

			if( isAdditionOrChange( delta ) ) {
        IHaskellParser parser = ParserManager.getInstance().getParser();
        ICompilationUnit unit = parser.parse( ( IFile )resource );
        if( unit.getModules().length > 0 ) {
          getLanguageModel().putModule( unit.getModules()[ 0 ] );
        }
      } else if( isDeletion( delta ) ) {
        // TODO we are assuming fileName = moduleName + .hs
        // is this always true?
        String moduleName = ResourceUtil.getModuleName( resource.getName() );
        getLanguageModel().removeModule( moduleName );
      }
			return true;
		}

		private boolean isAdditionOrChange(final IResourceDelta delta) {
			return    (delta.getKind() & ( IResourceDelta.ADDED
					                     | IResourceDelta.CHANGED))
				   != 0;
		}

		private boolean isDeletion(final IResourceDelta delta) {
			return (delta.getKind() & (IResourceDelta.REMOVED)) != 0;
		}

	}

	public IHaskellModel getLanguageModel() {
		return fLanguageModel;
	}
}