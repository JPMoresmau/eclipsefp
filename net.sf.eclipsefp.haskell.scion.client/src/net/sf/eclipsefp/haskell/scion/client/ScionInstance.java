package net.sf.eclipsefp.haskell.scion.client;

import java.io.Writer;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.client.CompilationResultHandler;
import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.internal.client.ScionServer;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckFileCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ConnectionInfoCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ListCabalComponentsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.LoadCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.NameDefinitionsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ThingAtPointCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Multiset;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.Component.ComponentType;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.osgi.util.NLS;

/**
 * Manages a single instance of the Scion server.
 * 
 * Objects from this class keep track of the state of the Scion server, so that the server
 * can be put into the same state after a restart (either of the server, or of the entire
 * workbench). 
 * 
 * @author Thomas ten Cate
 */
public class ScionInstance implements IScionCommandRunner {
	
	private String serverExecutable;
	
	private ScionServer server;
	
	private IProject project;
	
	private Multiset<IFile> loadedFiles = new Multiset<IFile>();
	
	private Writer serverOutput;
	
	public ScionInstance(String serverExecutable,IProject project,Writer serverOutput) {
		this.serverExecutable = serverExecutable;
		this.project=project;
		this.serverOutput=serverOutput;
	}

	public String getServerExecutable() {
		return serverExecutable;
	}
	
	public IProject getProject() {
		return project;
	}
	
	public void setServerExecutable(String serverExecutable) throws ScionServerStartupException {
		if (!this.serverExecutable.equals(serverExecutable)) {
			stop();
			this.serverExecutable = serverExecutable;
			start();
		}
	}
	
	public void start() throws ScionServerStartupException {
		if (server == null) {
			server = new ScionServer(serverExecutable,serverOutput);
			server.startServer();
			checkProtocol();
			//openCabal();
			restoreState();
		}
	}
	
	private boolean checkCabalFile(){
		IFile cabalFile=getCabalFile(getProject());
	    if( !cabalFile.exists() ) {
	        String id = ScionPlugin.ID_PROJECT_PROBLEM_MARKER;
	        try {
	        	IMarker marker = getProject().createMarker( id );
	        	marker.setAttribute( IMarker.MESSAGE, UITexts.bind(UITexts.cabalFileMissing, cabalFile.getLocation().toString()));
	        	marker.setAttribute( IMarker.SEVERITY, IMarker.SEVERITY_WARNING );
	        } catch (CoreException ce){
	        	ce.printStackTrace();
	        }
	    }
	    return cabalFile.exists();
	}
	
	/*public void configureCabal(IJobChangeListener listener) {
		if (checkCabalFile()){
			ConfigureCabalProjectCommand command=new ConfigureCabalProjectCommand(this,Job.BUILD,getProject());
			if (listener!=null){
				command.addJobChangeListener(listener);
			}
			command.runAsync();
		}
	}

	public void openCabal() {
		if (checkCabalFile()){
			OpenCabalProjectCommand command = new OpenCabalProjectCommand(this,Job.BUILD,getProject());
			command.addJobChangeListener(new JobChangeAdapter() {
				@Override
				public void done(IJobChangeEvent event) {
					if (event.getResult().isOK()) {
						configureCabal(null);
					}
				}
			});
			command.runAsync();
		}
		
	}*/
	
	public void buildProject(){
		deleteProblems(getProject());
		//configureCabal(new JobChangeAdapter(){
		//	@Override
		//	public void done(IJobChangeEvent event) {
		//		if (event.getResult().isOK()) {
		if (checkCabalFile()){
			final ListCabalComponentsCommand command=new ListCabalComponentsCommand(ScionInstance.this, Job.BUILD, getCabalFile(getProject()).getLocation().toOSString());
			command.addJobChangeListener(new JobChangeAdapter() {
				@Override
				public void done(IJobChangeEvent event) {
					if (event.getResult().isOK()) {
						for (Component c:command.getComponents()){
							LoadCommand loadCommand = new LoadCommand(ScionInstance.this,c,true);
							loadCommand.addJobChangeListener(new CompilationResultHandler(getProject()));
							loadCommand.runAsync();
						}
					}
				}
			});
			command.runAsync();
		}
		//		}
		//	}
		//});
	}

	public void stop() {
		if (server != null) {
			server.stopServer();
			server = null;
		}
	}
	
	////////////////////////////////
	// IScionCommandRunner methods
	
	public void runCommandSync(ScionCommand command,IProgressMonitor monitor) throws ScionServerException, ScionCommandException {
		if (server == null) {
			throw new ScionCommandException(command, UITexts.scionServerNotRunning_message);
		}
		try {
			server.runCommandSync(command,monitor);
		} catch (ScionServerException ex) {
			// fatal server error: restart
			stop();
			start();
			ScionPlugin.logWarning(UITexts.scionServerRestarted_message, ex);
			throw ex;
		}
	}
	
	public boolean contains(ISchedulingRule rule) {
		return rule == this || rule == getProject();
	}

	public boolean isConflicting(ISchedulingRule rule) {
		return rule == this;
	}
	
	//////////////////////
	// Internal commands
	
	private void checkProtocol() {
		
		ConnectionInfoCommand command = new ConnectionInfoCommand(this);
		command.addJobChangeListener(new JobChangeAdapter() {
			@Override
			public void done(IJobChangeEvent event) {
				if (event.getResult().isOK()) {
					ConnectionInfoCommand command = (ConnectionInfoCommand)event.getJob();
					if (command.getVersion() != ScionPlugin.PROTOCOL_VERSION) {
						ScionPlugin.logWarning(NLS.bind(UITexts.scionVersionMismatch_warning, Integer.toString(command.getVersion()), Integer.toString(ScionPlugin.PROTOCOL_VERSION)), null);
					}
				}
			}
		});
		command.runAsync();
	}
	
	private void restoreState() {
		for (IFile fileName : loadedFiles.uniqueSet()) {
			loadFile(fileName);
		}
	}
	
	//////////////////////
	// External commands

	public void backgroundTypecheckFile(IFile file) {
		deleteProblems(file);
		BackgroundTypecheckFileCommand command = new BackgroundTypecheckFileCommand(this, file);
		command.runAsync();
	}
	
	public void loadFile(IFile fileName) {
		loadedFiles.add(fileName);
		reloadFile(fileName);
	}
	
	private void deleteProblems(IResource r){
		try {
			r.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE );
		} catch( CoreException ex ) {
			ScionPlugin.logError(UITexts.error_deleteMarkers, ex);
			ex.printStackTrace();
		}
	}
	
	public void reloadFile(IFile file) {
		deleteProblems(file);
		LoadCommand loadCommand = new LoadCommand(this, new Component(ComponentType.FILE,file.getLocation().toOSString(),getCabalFile(getProject()).getLocation().toOSString()),false);
		BackgroundTypecheckFileCommand typecheckCommand = new BackgroundTypecheckFileCommand(this, file);
		typecheckCommand.addJobChangeListener(new CompilationResultHandler(getProject()));
		loadCommand.getSuccessors().add(typecheckCommand);
		loadCommand.runAsync();
		//typecheckCommand.runAsyncAfter(loadCommand);
	}
	

	
	public void unloadFile(IFile fileName) {
		// TODO TtC Scion has no command for unloading yet!
		loadedFiles.remove(fileName);
	}
	
	public String thingAtPoint(Location location) {
      ThingAtPointCommand command = new ThingAtPointCommand(this, location);
      command.runSync();
      if (command.getResult().isOK()) {
    	  return command.getThing();
      } else {
    	  return null;
      }
	}
	
	public Location firstDefinitionLocation(String name) {
		NameDefinitionsCommand command = new NameDefinitionsCommand(this, name);
		command.runSync();
		if (command.getResult().isOK() && command.isFound()) {
			return command.getFirstLocation();
		} else {
			return null;
		}
	}
	
	public static final String EXTENSION_CABAL = "cabal"; //$NON-NLS-1$
	
	public static IFile getCabalFile(final IProject p) {
	    String ext = EXTENSION_CABAL;
	    IPath path = new Path( p.getName() ).addFileExtension( ext );
	    return p.getFile( path );
	}
}
