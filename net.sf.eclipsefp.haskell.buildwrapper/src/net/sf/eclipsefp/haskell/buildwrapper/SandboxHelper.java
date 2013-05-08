/** 
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

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
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Display;

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
	
				Set<IProject> processed=new HashSet<IProject>();
				processed.add(p);
				for (IProject pR:p.getReferencedProjects()){
					installDeps(f,pR,processed);
				}
				
				LinkedList<String> args=new LinkedList<String>();
				args.add("install-deps");
				// enable tests
				args.add("--enable-tests");
				// force reinstalls since we won't break anything outside of the sandbox
				args.add("--force-reinstalls");
				f.runCabal(args);
				break;
			}
		}
	}

	/**
	 * install given project as a dependency on the given facade
	 * @param sandboxFacade the facade
	 * @param p the project
	 * @param processed the set of already processed projects, in case of loops
	 * @throws CoreException
	 */
	private static void installDeps(BWFacade sandboxFacade,IProject p,Set<IProject> processed) throws CoreException{
		if (sandboxFacade.isCanceled()){
			return;
		}
		if (processed.add(p)){
			for (IProject pR:p.getReferencedProjects()){
				installDeps(sandboxFacade,pR,processed);
			}
			if (sandboxFacade.isCanceled()){
				return;
			}
			LinkedList<String> args=new LinkedList<String>();
			args.add("install");
			args.add(p.getLocation().toOSString());
			args.add("--force-reinstalls");
			sandboxFacade.runCabal(args);
		}
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
				Set<IProject> processed=new HashSet<IProject>();
				processed.add(p);
				for (IProject pR:p.getReferencingProjects()){
					updateUsing(p,pR,processed);
				}
				break;
			}
		}
	}
	
	/**
	 * update a given project using a changed project
	 * @param changedProject the changed project
	 * @param p the project to update with the new version of the changed project
	 * @param processed the set of already processed projects, in case of loops
	 * @throws CoreException
	 */
	private static void updateUsing(IProject changedProject,IProject p,Set<IProject> processed) throws CoreException{
		if (processed.add(p)){
			BWFacade f=BuildWrapperPlugin.getFacade(p);
			
			if (f!=null && isSandboxed(f)){
				if (f.isCanceled()){
					return;
				}
				LinkedList<String> args=new LinkedList<String>();
				args.add("install");
				args.add(changedProject.getLocation().toOSString());
				args.add("--force-reinstalls");
				f.runCabal(args); // install the changed project 
				if (f.isCanceled()){
					return;
				}
				f.cleanGenerated(); // all generated files are wrong
				f.closeAllProcesses(); // GHC needs to reload the changes
				// rebuild if that's what the workspace wants
				if (f.isCanceled()){
					return;
				}
				if (ResourcesPlugin.getWorkspace().isAutoBuilding()){
					new JobFacade(f).build(new BuildOptions().setConfigure(true).setOutput(true).setRecompile(false));
				} else {
					// just configure
					f.configure(new BuildOptions().setConfigure(true));
				}
				//f.clean(new NullProgressMonitor());
			}
			for (IProject pR:p.getReferencingProjects()){
				updateUsing(changedProject,pR,processed);
			}
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
			/**
			 * if auto building, the building will see the project file has been changed and trigger installDeps accordingly
			 */
			if (!ResourcesPlugin.getWorkspace().isAutoBuilding()){
				try {
					event.getDelta().accept(new ProjectReferencesChangeVisitor());
				} catch (CoreException ce){
					BuildWrapperPlugin.logError(BWText.error_sandbox,ce);
				}
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
				final IFile fi=(IFile)delta.getResource();
				final IProject p=fi.getProject();
				// description was changed
				if (fi.getProjectRelativePath().toPortableString().equals(IProjectDescription.DESCRIPTION_FILE_NAME)){
					if (Display.findDisplay(Thread.currentThread())!=null){
						String name=NLS.bind(BWText.job_sandbox_deps, p.getName());
						Job installJob=new Job(name) {
							
							@Override
							protected IStatus run(IProgressMonitor arg0) {
								try {
									BWFacade f=BuildWrapperPlugin.getFacade(p);
									f.cabalFileChanged();
									installDeps(f);
								} catch (CoreException ce){
									BuildWrapperPlugin.logError(BWText.error_sandbox,ce);
								}
								return Status.OK_STATUS;
							}
						};
						installJob.setRule(p);
						installJob.setPriority(Job.BUILD);
						installJob.schedule();
					} else {
						try {
							BWFacade f=BuildWrapperPlugin.getFacade(p);
							f.cabalFileChanged();
							installDeps(f);
						} catch (CoreException ce){
							BuildWrapperPlugin.logError(BWText.error_sandbox,ce);
						}
					}
				}
				return false;
			}
			return true;
		}
	}
}
