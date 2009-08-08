package net.sf.eclipsefp.haskell.scion.client;

import net.sf.eclipsefp.haskell.scion.exceptions.ScionCommandException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerException;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.client.IScionCommandRunner;
import net.sf.eclipsefp.haskell.scion.internal.client.ScionServer;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckFileCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ConnectionInfoCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.LoadCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.NameDefinitionsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ScionCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ThingAtPointCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Multiset;
import net.sf.eclipsefp.haskell.scion.internal.util.UITexts;
import net.sf.eclipsefp.haskell.scion.types.Location;

import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
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
	
	private Multiset<String> loadedFiles = new Multiset<String>();
	
	public ScionInstance(String serverExecutable) {
		this.serverExecutable = serverExecutable;
	}

	public String getServerExecutable() {
		return serverExecutable;
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
			server = new ScionServer(serverExecutable);
			server.startServer();
			checkProtocol();
			restoreState();
		}
	}
	
	public void stop() {
		if (server != null) {
			server.stopServer();
			server = null;
		}
	}
	
	////////////////////////////////
	// IScionCommandRunner methods
	
	public void runCommandSync(ScionCommand command) throws ScionServerException, ScionCommandException {
		if (server == null) {
			throw new ScionCommandException(command, UITexts.scionServerNotRunning_message);
		}
		try {
			server.runCommandSync(command);
		} catch (ScionServerException ex) {
			// fatal server error: restart
			stop();
			start();
			ScionPlugin.logWarning(UITexts.scionServerRestarted_message, ex);
			throw ex;
		}
	}
	
	public boolean contains(ISchedulingRule rule) {
		return rule == this;
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
		for (String fileName : loadedFiles.uniqueSet()) {
			loadFile(fileName);
		}
	}
	
	//////////////////////
	// External commands

	public void backgroundTypecheckFile(String fileName) {
		BackgroundTypecheckFileCommand command = new BackgroundTypecheckFileCommand(this, fileName);
		command.runAsync();
	}
	
	public void loadFile(String fileName) {
		loadedFiles.add(fileName);
		reloadFile(fileName);
	}
	
	public void reloadFile(String fileName) {
		LoadCommand loadCommand = new LoadCommand(this, fileName);
		BackgroundTypecheckFileCommand typecheckCommand = new BackgroundTypecheckFileCommand(this, fileName);
		loadCommand.runAsync();
		typecheckCommand.runAsyncAfter(loadCommand);
	}
	
	public void unloadFile(String fileName) {
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
	
}
