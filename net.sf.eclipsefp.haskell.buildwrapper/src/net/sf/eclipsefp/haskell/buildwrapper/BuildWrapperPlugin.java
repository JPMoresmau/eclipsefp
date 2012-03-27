package net.sf.eclipsefp.haskell.buildwrapper;

import java.io.File;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.util.FileUtil;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
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
	
	// The shared instance
	private static BuildWrapperPlugin plugin;
	
	private static Map<IProject, BWFacade> facades=new HashMap<IProject, BWFacade>();
	
	private static String bwPath;
	
	public static boolean logAnswers=false;
	
	/**
	 * The constructor
	 */
	public BuildWrapperPlugin() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
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

	public static BWFacade createFacade(IProject p,String cabalPath,Writer outStream){

		IFile cf=getCabalFile(p);
		if (cf!=null){
			BWFacade f=new BWFacade();
			f.setBwPath(bwPath);
			f.setCabalPath(cabalPath);
			f.setCabalFile(cf.getLocation().toOSString());
			f.setWorkingDir(new File(p.getLocation().toOSString()));
			f.setProject(p);
			f.setOutStream(outStream);
			facades.put(p, f);
			new JobFacade(f).synchronize(false);
			return f;
		}
		return null;
	}
	
	public static BWFacade getFacade(IProject p){
		return facades.get(p);
	}
	
	public static BWFacade removeFacade(IProject p){
		return facades.remove(p);
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
	    log(Status.INFO, message, null);
	  }

	  public static void logDebug(String message) {
		// 	    log(Status.INFO, message, null);
	  }
	  
	  public static void logWarning(String message, Throwable cause) {
	    log(Status.WARNING, message, cause);
	  }


	  public static void logError(String message, Throwable cause) {
	    log(Status.ERROR, message, cause);
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
	    if (!r.getWorkspace().isTreeLocked() && r.exists() && r.getProject().isOpen()) {
	      try {
//	        if (r instanceof IFile) {
//	          r.refreshLocal(IResource.DEPTH_ZERO, new NullProgressMonitor());
//	        }
	        //org.eclipse.core.resources.problemmarker
	        r.deleteMarkers(PROBLEM_MARKER_ID, true, IResource.DEPTH_ZERO);
	        r.deleteMarkers("net.sf.eclipsefp.haskell.scion.client.ScionPlugin.projectProblem", true, IResource.DEPTH_ZERO);
	        r.deleteMarkers("net.sf.eclipsefp.haskell.core.scionProblem", true, IResource.DEPTH_ZERO);
	        r.deleteMarkers(IMarker.PROBLEM, false, IResource.DEPTH_ZERO); // delete problems but not subtypes (HLint, etc are not managed by us)
	      } catch (CoreException ex) {
	        BuildWrapperPlugin.logError(BWText.error_deleteMarkers, ex);
	        ex.printStackTrace();
	      }
	    }
	  }
	  
	  /**
	   * cache project cabal file if cabal file doesn't have same name than project
	   */
	  private static Map<IProject,IFile> m=new HashMap<IProject,IFile>();
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
	

	  
}
