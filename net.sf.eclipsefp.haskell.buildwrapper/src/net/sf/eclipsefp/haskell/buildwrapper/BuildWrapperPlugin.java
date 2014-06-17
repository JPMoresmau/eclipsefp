/**
 *  Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper;

import java.io.File;
import java.io.Writer;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.sf.eclipsefp.haskell.buildwrapper.types.BuildFlags;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalImplDetails;
import net.sf.eclipsefp.haskell.buildwrapper.usage.UsageAPI;
import net.sf.eclipsefp.haskell.buildwrapper.usage.UsageThread;
import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.OutputWriter;
import net.sf.eclipsefp.haskell.util.SingleJobQueue;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.statushandlers.StatusManager;
import org.osgi.framework.BundleContext;

/**
 * The plugin class for buildwrapper operations, providing utility methods and ways to obtain a Build Wrapper Facade for a given Haskell project
 * @author JPMoresmau
 */
public class BuildWrapperPlugin extends AbstractUIPlugin {
	final public static String PROBLEM_MARKER_ID = "net.sf.eclipsefp.haskell.core.problem";
	
	// The plug-in ID
	public static final String PLUGIN_ID = "net.sf.eclipsefp.haskell.buildwrapper";

	public static QualifiedName 				 USERFLAGS_PROPERTY=new QualifiedName("EclipseFP", "UserFlags");
	public static QualifiedName 				 EXTRAOPTS_PROPERTY=new QualifiedName("EclipseFP", "ExtraOpts");
	public static QualifiedName 				 EDITORSTANZA_PROPERTY=new QualifiedName("EclipseFP", "EditorStanza");
	
	// The shared instance
	private static BuildWrapperPlugin plugin;
	
	private static Map<IProject, BWFacade> facades=new HashMap<IProject, BWFacade>();
	
	private static String bwPath;
	private static int maxConfigureFailures=10;
	private static int maxEvalTime=30;
	
	public static boolean logAnswers=false;
	
	private UsageAPI usageAPI;
	private UsageThread usageThread=new UsageThread();
	
	private IResourceChangeListener sandboxListener=new SandboxHelper.ProjectReferencesChangeListener();
	
	private IResourceChangeListener nonHaskellListener=new NonHaskellResourceChangeListener();
	
	
	private IResourceChangeListener preDeleteListener = new IResourceChangeListener() {
		@Override
		public void resourceChanged(IResourceChangeEvent event) {
			IProject project = (IProject) event.getResource();
			// close all processes to prevent file-locking issues
			getFacade(project).closeAllProcesses();
		}
	};
	
	/**
	 * The constructor
	 */
	public BuildWrapperPlugin() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		usageThread.start();
		ResourcesPlugin.getWorkspace().addResourceChangeListener(sandboxListener,IResourceChangeEvent.POST_CHANGE);
		ResourcesPlugin.getWorkspace().addResourceChangeListener(preDeleteListener, IResourceChangeEvent.PRE_DELETE);
		ResourcesPlugin.getWorkspace().addResourceChangeListener(nonHaskellListener,IResourceChangeEvent.POST_CHANGE);
	}

	/**
	 * @param usageAPI the usageAPI to set
	 */
	public void setUsageAPI(UsageAPI usageAPI) {
		this.usageAPI = usageAPI;
	}
	
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(sandboxListener);
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(preDeleteListener);
		ResourcesPlugin.getWorkspace().removeResourceChangeListener(nonHaskellListener);
		// facades are removed and stopped by ScionManager
		
		usageThread.setShouldStop();
		// wait for all pending writes for 10 secs
		usageThread.join(10000);
		// then close api and db
		usageAPI.close();
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static BuildWrapperPlugin getDefault() {
		return plugin;
	}

	/**
	 * @return the usageAPI
	 */
	public UsageAPI getUsageAPI() {
		return usageAPI;
	}
	
	/**
	 * @return the usageThread
	 */
	public UsageThread getUsageThread() {
		return usageThread;
	}
	
	/**
	 * create a facade
	 * @param p the project
	 * @param impl the cabal invocations details
	 * @param outStream the writer to log to
	 * @return the created facade if the project has a cabal file, null otherwise
	 */
	public static BWFacade createFacade(IProject p,CabalImplDetails impl,Writer outStream){

		IFile cf=getCabalFile(p);
		if (cf!=null){
			BWFacade f=new BWFacade();
			f.setBwPath(bwPath);
			f.setCabalImplDetails(impl);
			f.setCabalFile(cf.getLocation().toOSString());
			f.setWorkingDir(new File(p.getLocation().toOSString()));
			f.setProject(p);
			f.setOutStream(outStream);
			facades.put(p, f);
			// why? build will do that for us
			// well if we don't build automatically we DO need it!
			if (!ResourcesPlugin.getWorkspace().isAutoBuilding()){
				new JobFacade(f).synchronize(false);
			}
			return f;
		}
		return null;
	}
	
	/**
	 * set cabal impl details on all known facades
	 * @param impl
	 */
	public static void setCabalImplDetails(CabalImplDetails impl){
		for (BWFacade f:facades.values()){
			f.setCabalImplDetails(impl);
		}
	}
	
	public static BWFacade getFacade(IProject p){
		return facades.get(p);
	}
	
	public static BWFacade removeFacade(IProject p){
		BWFacade f=facades.remove(p);
		if (f!=null){
			OutputWriter ow=f.getOutputWriter();
			if (ow!=null){
				ow.setTerminate();
			}
			f.getBuildJobQueue().close();
			f.getSynchronizeJobQueue().close();
			for (SingleJobQueue q:f.getThingAtPointJobQueues()){
				q.close();
			}
			for (SingleJobQueue q:f.getEditorSynchronizeJobQueues()){
				q.close();
			}
			

			f.closeAllProcesses();
		}
		return f;
	}
	
	public static JobFacade getJobFacade(IProject p){
		BWFacade realF=getFacade(p);
		if (realF!=null){
			return new JobFacade(realF);
		}
		return null;
	}
	
	public static WorkspaceFacade getWorkspaceFacade(IProject p,IProgressMonitor monitor){
		BWFacade realF=getFacade(p);
		if (realF!=null){
			return new WorkspaceFacade(realF,monitor);
		}
		return null;
	}
	
	 public static void logInfo(String message) {
	    log(IStatus.INFO, message, null);
	  }

	  public static void logDebug(String message) {
		// 	    log(Status.INFO, message, null);
	  }
	  
	  public static void logWarning(String message, Throwable cause) {
	    log(IStatus.WARNING, message, cause);
	  }


	  public static void logError(String message, Throwable cause) {
	    log(IStatus.ERROR, message, cause);
	  }

	  public static void log(int severity, String message, Throwable cause) {
	    Status status = new Status(severity, BuildWrapperPlugin.PLUGIN_ID, severity, message, cause);
	    logStatus(status);
	  }

	  public static void logStatus(IStatus status) {
	    StatusManager.getManager().handle(status);
	  }
	  
	  /**
	   * Delete all problem markers for a given file.
	   *  
	   * @param r A resource that could be a file or a project.
	   */
	  public static void deleteProblems(IResource r) {
	    deleteProblems(r, IResource.DEPTH_ZERO);
	  }
	  
	  private static void deleteProblems(IResource r,int depth){
		  if (!r.getWorkspace().isTreeLocked() && r.exists() && r.getProject().isOpen()) {
		      try {
//		        if (r instanceof IFile) {
//		          r.refreshLocal(IResource.DEPTH_ZERO, new NullProgressMonitor());
//		        }
		        //org.eclipse.core.resources.problemmarker
		        r.deleteMarkers(PROBLEM_MARKER_ID, true, depth);
		        r.deleteMarkers("net.sf.eclipsefp.haskell.scion.client.ScionPlugin.projectProblem", true, depth);
		        r.deleteMarkers("net.sf.eclipsefp.haskell.core.scionProblem", true,depth );
		        r.deleteMarkers(IMarker.PROBLEM, false, depth); // delete problems but not subtypes (HLint, etc are not managed by us)
		      } catch (CoreException ex) {
		        BuildWrapperPlugin.logError(BWText.error_deleteMarkers, ex);
		        ex.printStackTrace();
		      }
		    }
	  }
	  
	  public static void deleteAllProblems(IProject p) {
		  deleteProblems(p, IResource.DEPTH_INFINITE);
	  }
	  
	  /**
	   * cache project cabal file if cabal file doesn't have same name than project
	   */
	  private static Map<IProject,IFile> m=new HashMap<IProject,IFile>();
	  
	  /**
	   * get the cabal file path
	   * this is similar to getCabalFile below but does not cache the result and works with Path
	   * @param projectPath
	   * @param name
	   * @return
	   */
	  public static IPath getCabalFile(final IPath projectPath, final String name) {
		IPath f=projectPath.append(name).addFileExtension( FileUtil.EXTENSION_CABAL) ;
	    if (f==null || !f.toFile().exists()){ // oh oh

			// find a cabal file
    		File[] children=projectPath.toFile().listFiles();
    		int cnt=0;
    		for (File child:children){
    			if (child.isFile() && !child.isHidden()){
    				 IPath pchild=projectPath.append(child.getName());
    				 if ( pchild.getFileExtension() != null &&
    						 pchild.getFileExtension().equalsIgnoreCase(FileUtil.EXTENSION_CABAL)){
    					 f=pchild;
    					 cnt++;
    				 }
    			}
    		}
    		// cnt=1 would mean only one file, we can live with that
    		if (cnt!=1){
    			f=null;
    		}
	    		
	    }
	    return f;
	  }
	  
	  /**
	   * Generate the Cabal project file's name from the Eclipse project's name.
	   * 
	   * @param project The Eclipse project
	   * @return The "&lt;project&gt;.cabal" string.
	   */
	  public static IFile getCabalFile(final IProject project) {
	    IFile f=project.getFile(new Path(project.getName()).addFileExtension(FileUtil.EXTENSION_CABAL));
	    if (f==null || !f.exists()){ // oh oh
	    	IFile f2=m.get(project);
	    	if (f2==null){
	    		try {
	    			// find a cabal file
		    		IResource[] children=project.members();
		    		int cnt=0;
		    		for (IResource child:children){
		    			if (child instanceof IFile){
		    				 if ( child.getFileExtension() != null &&
		    						 child.getFileExtension().equalsIgnoreCase(FileUtil.EXTENSION_CABAL)){
		    					 f=(IFile)child;
		    					 cnt++;
		    				 }
		    			}
		    		}
		    		// cnt=1 would mean only one file, we can live with that
		    		if (cnt>1){
		    			// log error, we've taken a random cabal file
		    			logError(NLS.bind(BWText.project_cabal_duplicate, project.getName()),null);
		    		}
		    		m.put(project, f);
		    	} catch (CoreException ce){
		    		logError(NLS.bind(BWText.project_members_list_error, project.getName()), ce);
		    	}
	    	} else {
	    		f=f2;
	    	}
	    }
	    return f;
	  }

	public static String getBwPath() {
		return bwPath;
	}

	public static void setBwPath(String bwPath) {
		BuildWrapperPlugin.bwPath = bwPath;
		for (BWFacade f:facades.values()){
			if (f!=null){
				f.setBwPath(bwPath);
			}
		}
	}

	public static int getMaxConfigureFailures() {
		return maxConfigureFailures;
	}

	public static void setMaxConfigureFailures(int maxConfigureFailures) {
		BuildWrapperPlugin.maxConfigureFailures = maxConfigureFailures;
	}

	/**
	 * get location of cabal-dev global sandbox
	 * @return
	 */
	public static IPath getUniqueCabalDevSandboxLocation(){
	    return getDefault().getStateLocation().append( ".cabal-dev" );
	 }

	/**
	 * get location of cabal global sandbox
	 * @return
	 */
	 public static IPath getUniqueCabalSandboxLocation(){
	    return getDefault().getStateLocation().append( "sandbox" );
	 }

	public static int getMaxEvalTime() {
		return maxEvalTime;
	}

	public static void setMaxEvalTime(int maxEvalTime) {
		BuildWrapperPlugin.maxEvalTime = maxEvalTime;
	}

	/**
	 * get extensions used in a file
	 * @param file
	 * @return
	 */
	public static Set<String> getExtensions(IFile file){
		IProject p=file.getProject();
		BWFacade bwf=BuildWrapperPlugin.getFacade( p );
        Set<String> extensions=new HashSet<String>();
        if (bwf !=null){
          BuildFlags bf=bwf.getBuildFlags( file );
          if (bf!=null){
	          for (String s:bf.getGhcFlags()){
	            if (s.startsWith( "-X" )){
	              extensions.add(s.substring( 2 ));
	            }
	          }
          }
        }
        return extensions;
	}
}
