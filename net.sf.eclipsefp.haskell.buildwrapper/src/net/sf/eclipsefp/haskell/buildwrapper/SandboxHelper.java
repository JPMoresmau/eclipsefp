/** 
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper;

import java.util.LinkedList;

import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalImplDetails.SandboxType;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.osgi.util.NLS;

/**
 * Helper for sandbox operations
 * @author JP Moresmau
 *
 */
public class SandboxHelper {

	/**
	 * install dependencies of project using given facade in sandbox
	 * @param f
	 * @throws CoreException
	 */
	public static void installDeps(BWFacade f) throws CoreException{
		if (f!=null){
			SandboxType st=f.getCabalImplDetails().getType();
			switch (st){
			case CABAL_DEV:
				IProject p=f.getProject();
				for (IProject pR:p.getReferencedProjects()){
					installDeps(f,pR);
				}
				LinkedList<String> args=new LinkedList<String>();
				args.add("install-deps");
				f.runCabal(args);
				break;
			}
		}
	}

	/**
	 * install given project as a dependency on the given facade
	 * @param sandboxFacade the facade
	 * @param p the project
	 * @throws CoreException
	 */
	private static void installDeps(BWFacade sandboxFacade,IProject p) throws CoreException{
		for (IProject pR:p.getReferencedProjects()){
			installDeps(sandboxFacade,pR);
		}
		LinkedList<String> args=new LinkedList<String>();
		args.add("install");
		args.add(p.getLocation().toOSString());
		args.add("--force-reinstalls");
		sandboxFacade.runCabal(args);
	}
	
	/**
	 * update all projects using the project wrapped in the given facade
	 * @param f the facade
	 * @throws CoreException
	 */
	public static void updateUsing(BWFacade f) throws CoreException{
		if (f!=null){
			SandboxType st=f.getCabalImplDetails().getType();
			switch (st){
			case CABAL_DEV:
				IProject p=f.getProject();
				for (IProject pR:p.getReferencingProjects()){
					updateUsing(p,pR);
				}
				break;
			}
		}
	}
	
	/**
	 * update a given project using a changed project
	 * @param changedProject the changed project
	 * @param p the project to update with the new version of the changed project
	 * @throws CoreException
	 */
	private static void updateUsing(IProject changedProject,IProject p) throws CoreException{
		BWFacade f=BuildWrapperPlugin.getFacade(p);
		if (f!=null && isSandboxed(f)){
			LinkedList<String> args=new LinkedList<String>();
			args.add("install");
			args.add(changedProject.getLocation().toOSString());
			args.add("--force-reinstalls");
			f.runCabal(args); // install the changed project 
			f.cleanGenerated(); // all generated files are wrong
			f.configure(new BuildOptions().setConfigure(true));
			f.closeAllProcesses(); // GHC needs to reload the changes
			//f.clean(new NullProgressMonitor());
		}
		for (IProject pR:p.getReferencingProjects()){
			updateUsing(changedProject,pR);
		}

	}
	
	/**
	 * does a sandbox exists for the project wrapped in the given facade
	 * @param f
	 * @return
	 */
	public static boolean sandboxExists(BWFacade f){
		if (f!=null){
			SandboxType st=f.getCabalImplDetails().getType();
			switch (st){
			case CABAL_DEV:
				return f.getProject().getFolder(BWFacade.DIST_FOLDER_CABALDEV).exists();
			}
		}
		return false;
	}
	
	/**
	 * is the project sandboxed?
	 * @param f
	 * @return
	 */
	public static boolean isSandboxed(BWFacade f){
		if (f!=null){
			SandboxType st=f.getCabalImplDetails().getType();
			switch (st){
			case CABAL_DEV:
				return true;
			}
		}
		return false;
	}
	
	/**
	 * listen to changes on project file and update dependencies accordingly
	 * @author JP Moresmau
	 *
	 */
	public static class ProjectReferencesChangeListener implements IResourceChangeListener{
		/* (non-Javadoc)
		 * @see org.eclipse.core.resources.IResourceChangeListener#resourceChanged(org.eclipse.core.resources.IResourceChangeEvent)
		 */
		@Override
		public void resourceChanged( final IResourceChangeEvent event) {
			try {
				event.getDelta().accept(new ProjectReferencesChangeVisitor());
			} catch (CoreException ce){
				BuildWrapperPlugin.logError(BWText.error_sandbox,ce);
			}
		}
	}
	
	/**
	 * visitor for projects and definition file
	 * @author JP Moresmau
	 *
	 */
	private static class ProjectReferencesChangeVisitor implements IResourceDeltaVisitor {
		/* (non-Javadoc)
		 * @see org.eclipse.core.resources.IResourceDeltaVisitor#visit(org.eclipse.core.resources.IResourceDelta)
		 */
		@Override
		public boolean visit(IResourceDelta delta) throws CoreException {
			if (delta.getResource() instanceof IProject){
				if (IResourceDelta.CHANGED==delta.getKind()){
					if (isSandboxed(BuildWrapperPlugin.getFacade((IProject)delta.getResource()))){
						return true;
					}
				}
				return false;
			} else if (delta.getResource() instanceof IFile){
				final IFile f=(IFile)delta.getResource();
				final IProject p=f.getProject();
				// description was changed
				if (f.getProjectRelativePath().toPortableString().equals(IProjectDescription.DESCRIPTION_FILE_NAME)){
					String name=NLS.bind(BWText.job_sandbox_deps, p.getName());
					Job installJob=new Job(name) {
						
						@Override
						protected IStatus run(IProgressMonitor arg0) {
							try {
								installDeps(BuildWrapperPlugin.getFacade(p));
							} catch (CoreException ce){
								BuildWrapperPlugin.logError(BWText.error_sandbox,ce);
							}
							return Status.OK_STATUS;
						}
					};
					installJob.setRule(p );
					installJob.setPriority(Job.BUILD);
					installJob.schedule();
				}
				return false;
			}
			return true;
		}
	}
}
