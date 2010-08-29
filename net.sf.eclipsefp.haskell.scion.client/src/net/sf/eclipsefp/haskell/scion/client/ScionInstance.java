package net.sf.eclipsefp.haskell.scion.client;

import java.io.File;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.client.CompilationResultHandler;
import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.internal.client.ScionServer;
import net.sf.eclipsefp.haskell.scion.internal.commands.ArbitraryCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckArbitraryCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckFileCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.CabalDependenciesCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ConnectionInfoCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.DefinedNamesCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ListCabalComponentsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ListExposedModulesCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.LoadCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ModuleGraphCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.NameDefinitionsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.OutlineCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ParseCabalCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ThingAtPointCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.TokenTypesCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;
import net.sf.eclipsefp.haskell.scion.types.CabalPackage;
import net.sf.eclipsefp.haskell.scion.types.Component;
import net.sf.eclipsefp.haskell.scion.types.GhcMessages;
import net.sf.eclipsefp.haskell.scion.types.Location;
import net.sf.eclipsefp.haskell.scion.types.TokenDef;
import net.sf.eclipsefp.haskell.scion.types.Component.ComponentType;
import net.sf.eclipsefp.haskell.util.FileUtil;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
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
import org.json.JSONObject;

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
	
	private IFile loadedFile = null;
	
	private Writer serverOutput;
	
	private JSONObject cabalDescription;
	private Map<String,CabalPackage[]> packagesByDB;
	private List<Component> components=new LinkedList<Component>();
	private CabalComponentResolver resolver;
	private Component lastLoadedComponent;
	private List<String> exposedModulesCache=null;
	
	public ScionInstance(String serverExecutable,IProject project,Writer serverOutput,CabalComponentResolver resolver) {
		this.serverExecutable = serverExecutable;
		this.project=project;
		this.serverOutput=serverOutput;
		this.resolver=resolver;
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
		boolean exists=cabalFile.exists();
	    if( !exists) {
	    	String msg=UITexts.bind(UITexts.cabalFileMissing, cabalFile.getLocation().toString());
	    	ScionPlugin.logError(msg, null);
	    	if (!getProject().getWorkspace().isTreeLocked()){
		        String id = ScionPlugin.ID_PROJECT_PROBLEM_MARKER;
		        try {
		        	IMarker marker = getProject().createMarker( id );
		        	marker.setAttribute( IMarker.MESSAGE, msg);
		        	marker.setAttribute( IMarker.SEVERITY, IMarker.SEVERITY_WARNING );
		        } catch (CoreException ce){
		        	ScionPlugin.logError(msg, ce);
		        }
	    	} 
	    }
	    return exists;

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
		//configureCabal(new JobChangeAdapter(){
		//	@Override
		//	public void done(IJobChangeEvent event) {
		//		if (event.getResult().isOK()) {
		this.cabalDescription=null;
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
					deleteProblems(getProject());
					CompilationResultHandler crh=new CompilationResultHandler(getProject());
					components=command.getComponents();
					// if lastLoadedComponent is still present, load it last
					if (lastLoadedComponent!=null){
						List<Component> l=new ArrayList<Component>(components.size());
						Component toLoadLast=null;
						for (Component c:components){
							if (c.toString().equals(lastLoadedComponent.toString())){
								toLoadLast=c;
							} else {
								l.add(c);
							}
						}
						if (toLoadLast!=null){
							l.add(toLoadLast);
						}
						components=l;
					}
					for (Component c:components){
						LoadCommand loadCommand = new LoadCommand(ScionInstance.this,c,output);
						//loadCommand.addJobChangeListener();
						loadCommand.run(monitor);
						crh.process(loadCommand);
						lastLoadedComponent=c;
					}
					
					ParseCabalCommand pcc=new ParseCabalCommand(ScionInstance.this,getCabalFile(getProject()).getLocation().toOSString());
					pcc.run(monitor);
					ScionInstance.this.cabalDescription=pcc.getDescription();
					
					CabalDependenciesCommand cdc=new CabalDependenciesCommand(ScionInstance.this,getCabalFile(getProject()).getLocation().toOSString());
					cdc.run(monitor);
					ScionInstance.this.packagesByDB=cdc.getPackagesByDB();
					
					restoreState();
					
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
		cabalDescription=null;
		components.clear();
		packagesByDB=null;
		lastLoadedComponent=null;
		exposedModulesCache=null;
		if (server != null) {
			server.stopServer();
			server = null;
		}
	}
	
	public boolean isStopped(){
		return server==null;
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
		return rule == this || rule == getProject() || (getProject().contains(rule));
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
		if (loadedFile!=null){
			loadFile(loadedFile,false);
		}
	}
	
	//////////////////////
	// External commands

	/*public void backgroundTypecheckFile(IFile file) {
		BackgroundTypecheckFileCommand command = new BackgroundTypecheckFileCommand(this, file);
		command.runAsync();
	}*/

	/*public void backgroundTypecheckArbitrary(final IFile file,IDocument doc) {
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
	}*/
	
	public void loadFile(IFile fileName,boolean sync) {
		//loadedFiles.add(fileName);
		reloadFile(fileName,null,sync);
	}
	
	public IFile getLoadedFile() {
		return loadedFile;
	}
	
	public boolean isLoaded(IFile f) {
		return f.equals(loadedFile);
	}
	
	public void setLoadedFile(IFile loadedFile) {
		this.loadedFile = loadedFile;
	}
	
	public void deleteProblems(IResource r){
		if (!r.getWorkspace().isTreeLocked() && r.exists() && r.getProject().isOpen()){
			try {
				if (r instanceof IFile){
					r.refreshLocal(IResource.DEPTH_ZERO, new NullProgressMonitor());
				} 
				r.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE );
			} catch( CoreException ex ) {
				ScionPlugin.logError(UITexts.error_deleteMarkers, ex);
				ex.printStackTrace();
			}
		}
	}
	
	public void reloadFile(final IFile file,final Runnable after,final boolean sync) {
		// done on return
		//deleteProblems(file);
		//LoadCommand loadCommand = new LoadCommand(this, new Component(ComponentType.FILE,file.getLocation().toOSString(),getCabalFile(getProject()).getLocation().toOSString()),false);
		
		Set<String> componentNames=resolver.getComponents(file);
		if (lastLoadedComponent==null || !componentNames.contains(lastLoadedComponent.toString())){
			Component toLoad=null;
			// we have no component: we create a file one
			if (componentNames.isEmpty()){
				toLoad=new Component(ComponentType.FILE, file.getName(), file.getLocation().toOSString());
			} else {
				for (final Component compo:components){
					if (componentNames.contains(compo.toString())){
						toLoad=compo;
						break;
	
					}
				}
			}
			if (toLoad!=null){
				final Component compo=toLoad;
				LoadCommand loadCommand = new LoadCommand(ScionInstance.this,compo,false);
				run(loadCommand,new Runnable() {
					
					public void run() {
						lastLoadedComponent=compo;
						BackgroundTypecheckFileCommand cmd = new BackgroundTypecheckFileCommand(ScionInstance.this, file);
						//cmd.addJobChangeListener(new CompilationResultHandler(getProject()));
						ScionInstance.this.run(cmd,after,sync);
					}
				},sync);
				return;		
			}
		} 
		
		BackgroundTypecheckFileCommand cmd = new BackgroundTypecheckFileCommand(this, file);
		//cmd.addJobChangeListener(new CompilationResultHandler(getProject()));
		run(cmd,after,sync);
		
		//loadCommand.getSuccessors().add(typecheckCommand);
		//loadCommand.runAsync();
		//typecheckCommand.runAsyncAfter(loadCommand);
	}
	
	public void reloadFile(final IFile file,final IDocument doc,final Runnable after,final boolean sync) {
		// done on return
		//deleteProblems(file);
		//LoadCommand loadCommand = new LoadCommand(this, new Component(ComponentType.FILE,file.getLocation().toOSString(),getCabalFile(getProject()).getLocation().toOSString()),false);
		//final IJobChangeListener l=new CompilationResultHandler(getProject(),doc);
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
				if (message!=null && message.contains(GhcMessages.ERROR_INTERACTIVE_DISABLED)){
					deleteProblems(file);
					ScionPlugin.logWarning(UITexts.bind(UITexts.warning_typecheck_arbitrary_failed,message), null);
					//removeJobChangeListener(l);
					removeJobChangeListener(l2);
					ScionInstance.this.reloadFile(file, after,sync);
					
					return true;
				} 
				return super.onError(ex, name, message);
				
			}
		};
		//cmd.addJobChangeListener(l);
		cmd.addJobChangeListener(l2);
		run(cmd,null,sync);
		//loadCommand.getSuccessors().add(typecheckCommand);
		//loadCommand.runAsync();
		//typecheckCommand.runAsyncAfter(loadCommand);
	}
	
	private void run(ScionCommand cmd,final Runnable after,boolean sync){
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
		if (sync){
			cmd.runSync();
		} else {
			cmd.runAsync();
		}
	}
	
	public void unloadFile(IFile fileName) {
		if (fileName.equals(loadedFile)){
			loadedFile=null;
		}
	}
	
	public String thingAtPoint(Location location) {

		ThingAtPointCommand command = new ThingAtPointCommand(
				ScionInstance.this, location);
		command.runSync();
		if (command.getResult().isOK()) {
			return command.getThing();
		} else {
			return null;
		}

	}
		
	public void outline(final IFile file,final OutlineHandler handler,final boolean sync){
		withLoadedFile(file, new Runnable(){
			public void run() {
				final OutlineCommand command=new OutlineCommand(file,ScionInstance.this);
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
				ScionInstance.this.run(command,null,sync);
				
			}
		},sync);
	}

	public void withLoadedFile(final IFile file,Runnable run,final boolean sync){
		if (isLoaded(file)){
			run.run();
		} else {
			reloadFile(file, run,sync);
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
	
	public static IFile getCabalFile(final IProject p) {
	    String ext = FileUtil.EXTENSION_CABAL;
	    IPath path = new Path( p.getName() ).addFileExtension( ext );
	    return p.getFile( path );
	}

	public JSONObject getCabalDescription() {
		return cabalDescription;
	}
	
	public Map<String, CabalPackage[]> getPackagesByDB() {
		return packagesByDB;
	}
	
	public List<Component> getComponents() {
		return components;
	}
	
	public void definedNames(final NameHandler handler){
		
		if (handler!=null){
			final DefinedNamesCommand command=new DefinedNamesCommand(this);
			command.addJobChangeListener(new JobChangeAdapter(){
				@Override
				public void done(IJobChangeEvent event) {
					if (event.getResult().isOK()) {
						handler.nameResult(command.getNames());
					}
				}
			});
			command.runSync();
		}
		

	}
	
	public void listExposedModules(final NameHandler handler){
		if (handler!=null){
			if (exposedModulesCache!=null){
				handler.nameResult(exposedModulesCache);
				return;
			}
			final ListExposedModulesCommand command=new ListExposedModulesCommand(this);
		
			command.addJobChangeListener(new JobChangeAdapter(){
				@Override
				public void done(IJobChangeEvent event) {
					if (event.getResult().isOK()) {
						exposedModulesCache=Collections.unmodifiableList(command.getNames());
						handler.nameResult(exposedModulesCache);
					}
				}
			});
		
			command.runSync();
		}

	}
	
	public void moduleGraph(final NameHandler handler){
		if (handler!=null){
			
			final ModuleGraphCommand command=new ModuleGraphCommand(this);
		
			command.addJobChangeListener(new JobChangeAdapter(){
				@Override
				public void done(IJobChangeEvent event) {
					if (event.getResult().isOK()) {
						handler.nameResult(command.getNames());
					}
				}
			});
			command.runSync();
		}
		

	}
	
	public List<TokenDef> tokenTypes(IFile file,String contents){
		if (cabalDescription!=null){
			TokenTypesCommand command=new TokenTypesCommand(this, file, contents,FileUtil.hasLiterateExtension(file));
			command.runSync();
			return command.getTokens();
		}
		return null;
	}
}
