/** 
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper;

import java.io.File;
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
import org.eclipse.core.runtime.NullProgressMonitor;
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
		installDeps(f, new HashSet<BWFacade>());
	}
	
	public static void installDeps(BWFacade f,Set<BWFacade> processedFacades) throws CoreException{
		
		if (f!=null && processedFacades.add(f)){
			SandboxType st=f.getCabalImplDetails().getType();
			IProject p=f.getProject();
			
			switch (st){
				case CABAL_DEV:
				{
					if (!f.getCabalImplDetails().isUniqueSandbox()){
						Set<IProject> processed=new HashSet<>();
						processed.add(p);
						for (IProject pR:p.getReferencedProjects()){
							installDeps(f,pR,processed);
						}
					}
					
					LinkedList<String> args=new LinkedList<>();
					args.add("install-deps");
					// enable tests
					args.add("--enable-tests");
					// enable benchmarks
					args.add("--enable-benchmarks");
					// force reinstalls since we won't break anything outside of the sandbox
					args.add("--force-reinstalls");
					
					args.addAll(f.getCabalImplDetails().getInstallOptions());
					f.runCabal(args,null);
					break;
				}
				case CABAL:
				{
					// init unique sandbox
					if (f.getCabalImplDetails().isUniqueSandbox() && !f.getSandboxPath().exists()){
						LinkedList<String> args=new LinkedList<>();
						args.add("sandbox");
						args.add("init");
						args.add("--sandbox="+ f.getSandboxPath());
						f.runCabal(args,"",f.getSandboxPath());
						
					}
					if (!p.getFile("cabal.sandbox.config").exists()){
						LinkedList<String> args=new LinkedList<>();
						args.add("sandbox");
						args.add("init");
						args.addAll(f.getCabalImplDetails().getInitOptions());
						f.runCabal(args,"",null);
					}
					
					//if (!f.getCabalImplDetails().isUniqueSandbox()){
						Set<IProject> processed=new HashSet<>();
						processed.add(p);
						for (IProject pR:p.getReferencedProjects()){
							addSource(f,pR,processed);
						}
					//}
					// Cabal doesn't allow installing only-dependencies if the project has dependent projects in the same sandbox, so call installDeps on dependent
					if (f.getCabalImplDetails().isUniqueSandbox()){
						IProject[] refs=p.getReferencingProjects();
						if (refs.length>0){
							for(IProject r:refs){
								BWFacade rf=BuildWrapperPlugin.getFacade(r);
								if (rf!=null && !rf.isCanceled()){
									installDeps(rf,processedFacades);
								}
							}
							return;
						}
					}
					
					LinkedList<String> args=new LinkedList<>();
					args.add("install");
					args.add("--only-dependencies");
					// enable tests
					args.add("--enable-tests");
					// enable benchmarks
					args.add("--enable-benchmarks");
					// force reinstalls since we won't break anything outside of the sandbox
					args.add("--force-reinstalls");
					args.addAll(f.getCabalImplDetails().getInstallOptions());
					
					f.runCabal(args,null);
					break;
				}
				case NONE:
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
			LinkedList<String> args=new LinkedList<>();
			args.add("install");
			args.add(p.getLocation().toOSString());
			args.add("--force-reinstalls");
			//if (SandboxType.CABAL_DEV.equals(sandboxFacade.getCabalImplDetails().getType())){
			//if (s)
				args.addAll(sandboxFacade.getCabalImplDetails().getInstallOptions());
			//}
			sandboxFacade.runCabal(args,null);
		}
	}
	
	/**
	 * install given project as a dependency on the given facade
	 * @param sandboxFacade the facade
	 * @param p the project
	 * @param processed the set of already processed projects, in case of loops
	 * @throws CoreException
	 */
	private static void addSource(BWFacade sandboxFacade,IProject p,Set<IProject> processed) throws CoreException{
		if (sandboxFacade.isCanceled() || !p.isAccessible()){
			return;
		}
		if (processed.add(p)){
			for (IProject pR:p.getReferencedProjects()){
				addSource(sandboxFacade,pR,processed);
			}
			if (sandboxFacade.isCanceled()){
				return;
			}
			Set<IProject> addSources=sandboxFacade.getCabalImplDetails().isUniqueSandbox()
					?BuildWrapperPlugin.getAddSourceProjects()
					:sandboxFacade.getAddSourceProjects();
			boolean lastAdd=addSources.contains(p);
			// add-source keeps track of modifications, so we only add once in the session to be sure
			if (!lastAdd){
				LinkedList<String> args=new LinkedList<>();
				args.add("sandbox");
				args.add("add-source");
				args.add(p.getLocation().toOSString());
				
				sandboxFacade.runCabal(args,null);
				addSources.add(p);
			}
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
			Set<IProject> processed=new HashSet<>();
			switch (st){
			case CABAL_DEV:
				if (!f.getCabalImplDetails().isUniqueSandbox()){
					IProject p=f.getProject();
					processed.add(p);
					for (IProject pR:p.getReferencingProjects()){
						updateUsing(p,pR,processed);
					}
				} else {
					IProject p=f.getProject();
					if (install(p,p,processed)){
						for (IProject pR:p.getReferencingProjects()){
							build(p,pR,processed);
						}
					}
				}
				break;
			case CABAL:
				// in Cabal sandboxes, we have automatic updates
				// either we build in the common sandbox, or we use addSource which maintain up to date packages
				// we let Eclipse manage this: if we build manually we don't want to build everything...
//				IProject p=f.getProject();
//				for (IProject pR:p.getReferencingProjects()){
//					build(p,pR,processed);
//				}
				break;
			case NONE:
				break;
			}
		}
	}
	
	/**
	 * install the changedProject in p's sandbox
	 * @param changedProject the changed project
	 * @param p the project to update with the new version of the changed project
	 * @param processed the set of already processed projects, in case of loops
	 * @return true if the install in a sandbox happened
	 */
	private static boolean install(IProject changedProject,IProject p,Set<IProject> processed){
		if (processed.add(p)){
			BWFacade f=BuildWrapperPlugin.getFacade(p);
			
			if (f!=null && isSandboxed(f)){
				if (f.isCanceled()){
					return false;
				}
				LinkedList<String> args=new LinkedList<>();
				args.add("install");
				args.add(changedProject.getLocation().toOSString());
				args.add("--force-reinstalls");
				String expFlags=null;
				BWFacade changedF=BuildWrapperPlugin.getFacade(changedProject);
				if (changedF!=null){
					expFlags=changedF.getFlags();
				}
				f.runCabal(args,expFlags,null); // install the changed project with its own flags
				if (f.isCanceled()){
					return false;
				}
				f.cleanGenerated(); // all generated files are wrong
				f.closeAllProcesses(); // GHC needs to reload the changes
				// rebuild if that's what the workspace wants
				if (f.isCanceled()){
					return false;
				}
				return true;
				//f.clean(new NullProgressMonitor());
			}
		}
		return false;
	}
	
	/**
	 * build or configure a given project
	 * @param p
	 */
	private static void build(IProject p){
		
		BWFacade f=BuildWrapperPlugin.getFacade(p);
		if (f.isCanceled()){
			return;
		}
		if (ResourcesPlugin.getWorkspace().isAutoBuilding()){
			new JobFacade(f).build(new BuildOptions().setConfigure(true).setOutput(true).setRecompile(false));
		} else {
			// just configure
			f.configure(new BuildOptions().setConfigure(true));
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
		if (install(changedProject,p,processed)){
			build(p);
			for (IProject pR:p.getReferencingProjects()){
				updateUsing(changedProject,pR,processed);
			}
			
		}
	}
	
	/**
	 * build a project and the referencing projects
	 * @param changedProject
	 * @param p
	 * @param processed
	 * @throws CoreException
	 */
	private static void build(IProject changedProject,IProject p,Set<IProject> processed) throws CoreException{
		if (processed.add(p)){
			build(p);
			for (IProject pR:p.getReferencingProjects()){
				build(changedProject,pR,processed);
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
			boolean uq=f.getCabalImplDetails().isUniqueSandbox();
			switch (st){
			case CABAL_DEV:
				return 
						uq?new File(f.getCabalImplDetails().getSandboxPath()).exists()
						:f.getProject().getFolder(BWFacade.DIST_FOLDER_CABALDEV).exists();
			case CABAL:
				return uq?new File(f.getCabalImplDetails().getSandboxPath()).exists()
						:f.getProject().getFolder(".cabal-sandbox").exists();
			case NONE:
				return false;
			}
		}
		return false;
	}
	
	public static void sandboxLocationChanged(BWFacade f){
		if (f!=null){
			SandboxType st=f.getCabalImplDetails().getType();
			if (SandboxType.CABAL.equals(st)){
				try {
					f.getProject().getFile("cabal.sandbox.config").delete(true, new NullProgressMonitor());
				} catch (CoreException ce){
					BuildWrapperPlugin.logError(BWText.error_sandbox,ce);
				}
			}
		}
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
			case CABAL:	
				return true;
			case NONE:
				return false;
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
