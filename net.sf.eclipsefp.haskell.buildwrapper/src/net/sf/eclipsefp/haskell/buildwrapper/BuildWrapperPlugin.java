package net.sf.eclipsefp.haskell.buildwrapper;

import java.io.File;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;

import net.sf.eclipsefp.haskell.buildwrapper.util.BWText;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;


import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.statushandlers.StatusManager;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class BuildWrapperPlugin extends AbstractUIPlugin {
	final static String PROBLEM_MARKER_ID = "net.sf.eclipsefp.haskell.core.problem";
	
	// The plug-in ID
	public static final String PLUGIN_ID = "net.sf.eclipsefp.haskell.buildwrapper";

	// The shared instance
	private static BuildWrapperPlugin plugin;
	
	private static Map<IProject, BWFacade> facades=new HashMap<IProject, BWFacade>();
	
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

	public static BWFacade createFacade(IProject p,String bwPath,String cabalPath,Writer outStream){

		IFile cf=ScionInstance.getCabalFile(p);
		if (cf!=null){
			BWFacade f=new BWFacade();
			f.setBwPath(bwPath);
			f.setCabalPath(cabalPath);
			f.setCabalFile(cf.getLocation().toOSString());
			f.setWorkingDir(new File(p.getLocation().toOSString()));
			f.setOutStream(outStream);
			f.setProject(p);
			facades.put(p, f);
			return f;
		}
		return null;
	}
	
	public static IBWFacade getFacade(IProject p){
		return facades.get(p);
	}
	
	public static IBWFacade getJobFacade(IProject p){
		IBWFacade realF=getFacade(p);
		if (realF!=null){
			return new JobFacade(realF);
		}
		return null;
	}
	
	public static IBWFacade getWorkspaceFacade(IProject p,IProgressMonitor monitor){
		IBWFacade realF=getFacade(p);
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
	    Status status = new Status(severity, ScionPlugin.getPluginId(), severity, message, cause);
	    logStatus(status);
	  }

	  public static void logStatus(IStatus status) {
	    StatusManager.getManager().handle(status);
	  }
	  
	  /**
	   * Delete all problem markers for a given file.
	   *  
	   * @param r A resource that should be a file.
	   */
	  public static void deleteProblems(IResource r) {
	    if (!r.getWorkspace().isTreeLocked() && r.exists() && r.getProject().isOpen()) {
	      try {
	        if (r instanceof IFile) {
	          r.refreshLocal(IResource.DEPTH_ZERO, new NullProgressMonitor());
	        }
	        r.deleteMarkers(PROBLEM_MARKER_ID, true, IResource.DEPTH_ZERO);
	        r.deleteMarkers(ScionPlugin.ID_PROJECT_PROBLEM_MARKER, true, IResource.DEPTH_ZERO);
	      } catch (CoreException ex) {
	        ScionPlugin.logError(BWText.error_deleteMarkers, ex);
	        ex.printStackTrace();
	      }
	    }
	  }
}
