/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper;

import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.util.FileUtil;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;

/**
 * Listens to changes to non haskell resources
 * @author JP Moresmau
 * 
 */
public class NonHaskellResourceChangeListener implements
		IResourceChangeListener {

	/**
	 * 
	 */
	public NonHaskellResourceChangeListener() {

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org
	 * .eclipse.core.resources.IResourceChangeEvent)
	 */
	@Override
	public void resourceChanged(IResourceChangeEvent event) {
		try {
			event.getDelta().accept(new IResourceDeltaVisitor() {
				@Override
				public boolean visit(final IResourceDelta delta) {
					if (delta.getKind() == IResourceDelta.CHANGED
							&& (delta.getFlags() & IResourceDelta.CONTENT) > 0) {
						if (delta.getResource() instanceof IFile) {
							IFile f = (IFile) delta.getResource();
							IProject prj = f.getProject();
							BWFacade bwf = BuildWrapperPlugin.getFacade(prj);
							if (bwf != null) {
								if (!FileUtil.hasAnySourceExtension(f)) {
									bwf.synchronize1(f, false);
								}
							}
							return false;
						}
					} if (delta.getResource().getName().equals(BWFacade.DIST_FOLDER)){
					  return false;
					}

					return true;

				}
			});

		} catch (CoreException ex) {
			BuildWrapperPlugin.logError(BWText.error_listener_nonhaskell, ex);
		}

	}

}
