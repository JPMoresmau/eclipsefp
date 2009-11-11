package net.sf.eclipsefp.haskell.scion.client;

import java.io.File;
import java.io.Writer;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.client.CompilationResultHandler;
import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.internal.client.ScionServer;
import net.sf.eclipsefp.haskell.scion.internal.commands.ArbitraryCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckArbitraryCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckFileCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ConnectionInfoCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ListCabalComponentsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.LoadCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.NameDefinitionsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.OutlineCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ThingAtPointCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Multiset;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.Location;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.text.IDocument;
import org.eclipse.osgi.util.NLS;
import org.json.JSONException;

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
			File directory=new File(project.getLocation().toOSString());
			server = new ScionServer(serverExecutable,serverOutput,directory);
			server.startServer();
			checkProtocol();
			//openCabal();
			buildProject(false);
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
	
	public void buildProject(final boolean output){
		deleteProblems(getProject());
		//configureCabal(new JobChangeAdapter(){
		//	@Override
		//	public void done(IJobChangeEvent event) {
		//		if (event.getResult().isOK()) {
		if (checkCabalFile()){
			final ListCabalComponentsCommand command=new ListCabalComponentsCommand(ScionInstance.this, Job.BUILD, getCabalFile(getProject()).getLocation().toOSString());
			/*command.addJobChangeListener(new JobChangeAdapter() {
				@Override
				public void done(IJobChangeEvent event) {
					if (event.getResult().isOK()) {
						for (Component c:command.getComponents()){
							LoadCommand loadCommand = new LoadCommand(ScionInstance.this,c,output);
							loadCommand.addJobChangeListener(new CompilationResultHandler(getProject()));
							loadCommand.runSync();
						}
					}
				}
			});*/
			command.getSuccessors().add(new ArbitraryCommand(ScionInstance.this, Job.BUILD){
				@Override
				public IStatus run(IProgressMonitor monitor) {
					CompilationResultHandler crh=new CompilationResultHandler(getProject());
					
					for (Component c:command.getComponents()){
						LoadCommand loadCommand = new LoadCommand(ScionInstance.this,c,output);
						//loadCommand.addJobChangeListener();
						loadCommand.run(monitor);
						crh.process(loadCommand);
					}
					return Status.OK_STATUS;
				}
			});
			//if (output){
				command.runAsync();
			//} else {
			//	command.runSync();
			//}
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
		BackgroundTypecheckFileCommand command = new BackgroundTypecheckFileCommand(this, file);
		command.runAsync();
	}

	public void backgroundTypecheckArbitrary(final IFile file,IDocument doc) {
		BackgroundTypecheckArbitraryCommand cmd = new BackgroundTypecheckArbitraryCommand(this, file,doc);
		cmd.addJobChangeListener(new JobChangeAdapter(){
			@Override
			public void done(IJobChangeEvent event) {
				if (!event.getResult().isOK()){
					ScionInstance.this.reloadFile(file, null);
				}
			}
		});
		cmd.runAsync();
	}
	
	public void loadFile(IFile fileName) {
		//loadedFiles.add(fileName);
		reloadFile(fileName,null);
	}
	
	public Multiset<IFile> getLoadedFiles() {
		return loadedFiles;
	}
	
	public boolean isLoaded(IFile f) {
		return loadedFiles.contains(f);
	}
	
	
	public void deleteProblems(IResource r){
		if (!r.getWorkspace().isTreeLocked()){
			try {
				r.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE );
			} catch( CoreException ex ) {
				ScionPlugin.logError(UITexts.error_deleteMarkers, ex);
				ex.printStackTrace();
			}
		}
	}
	
	public void reloadFile(IFile file,Runnable after) {
		deleteProblems(file);
		//LoadCommand loadCommand = new LoadCommand(this, new Component(ComponentType.FILE,file.getLocation().toOSString(),getCabalFile(getProject()).getLocation().toOSString()),false);
		BackgroundTypecheckFileCommand cmd = new BackgroundTypecheckFileCommand(this, file);
		cmd.addJobChangeListener(new CompilationResultHandler(getProject()));
		runAsync(cmd,after);
		//loadCommand.getSuccessors().add(typecheckCommand);
		//loadCommand.runAsync();
		//typecheckCommand.runAsyncAfter(loadCommand);
	}
	
	public void reloadFile(final IFile file,final IDocument doc,final Runnable after) {
		deleteProblems(file);
		
		//LoadCommand loadCommand = new LoadCommand(this, new Component(ComponentType.FILE,file.getLocation().toOSString(),getCabalFile(getProject()).getLocation().toOSString()),false);
		final IJobChangeListener l=new CompilationResultHandler(getProject(),doc);
		final IJobChangeListener l2=new JobChangeAdapter(){
				@Override
				public void done(IJobChangeEvent event) {
					
					if (event.getResult().isOK()) {
						if (after!=null){
							after.run();
						}
					}
				}
			};
		BackgroundTypecheckArbitraryCommand cmd = new BackgroundTypecheckArbitraryCommand(this, file,doc){
			@Override
			protected boolean onError(JSONException ex, String name, String message) {
				ScionPlugin.logWarning(UITexts.bind(UITexts.warning_typecheck_arbitrary_failed,message), null);
				removeJobChangeListener(l);
				removeJobChangeListener(l2);
				ScionInstance.this.reloadFile(file, after);
				return true;
			}
		};
		cmd.addJobChangeListener(l);
		cmd.addJobChangeListener(l2);
		cmd.runAsync();
		//loadCommand.getSuccessors().add(typecheckCommand);
		//loadCommand.runAsync();
		//typecheckCommand.runAsyncAfter(loadCommand);
	}
	
	private void runAsync(ScionCommand cmd,final Runnable after){
		if (after!=null){
			cmd.addJobChangeListener(new JobChangeAdapter(){
				@Override
				public void done(IJobChangeEvent event) {
					
					if (event.getResult().isOK()) {
						after.run();
					}
				}
			});
		}
		cmd.runAsync();
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
	
	public void outline(final IFile file,final OutlineHandler handler){
		final OutlineCommand command=new OutlineCommand(file,this);
		if (handler!=null){
			command.addJobChangeListener(new JobChangeAdapter(){
				@Override
				public void done(IJobChangeEvent event) {
					if (event.getResult().isOK()) {
						handler.outlineResult(command.getOutlineDefs());
					}
				}
			});
		}
		command.runAsync();

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
