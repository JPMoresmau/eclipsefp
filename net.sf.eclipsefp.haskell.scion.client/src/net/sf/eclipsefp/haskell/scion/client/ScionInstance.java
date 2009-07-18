package net.sf.eclipsefp.haskell.scion.client;

import net.sf.eclipsefp.haskell.scion.internal.client.ScionThreadManager;
import net.sf.eclipsefp.haskell.scion.internal.commands.BackgroundTypecheckFileCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ConnectionInfoCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.LoadCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.NameDefinitionsCommand;
import net.sf.eclipsefp.haskell.scion.internal.commands.ThingAtPointCommand;
import net.sf.eclipsefp.haskell.scion.internal.util.Multiset;
import net.sf.eclipsefp.haskell.scion.types.Location;

/**
 * Manages a single instance of the Scion server.
 * 
 * This is one level above {@link ScionThreadManager}, in the sense that it is concerned
 * with the semantics of the commands, instead of the mechanics of sending and receiving.
 * 
 * Objects from this class keep track of the state of the Scion server, so that the server
 * can be put into the same state after a restart (either of the server, or of the entire
 * workbench). 
 * 
 * @author Thomas ten Cate
 */
public class ScionInstance {
	
	private String serverExecutable;
	private ScionThreadManager threadManager;
	private Multiset<String> loadedFiles = new Multiset<String>();
	
	public ScionInstance(String serverExecutable) {
		this.serverExecutable = serverExecutable;
		createThreadManager();
	}

	public String getServerExecutable() {
		return serverExecutable;
	}
	
	public void setServerExecutable(String serverExecutable) {
		if (!this.serverExecutable.equals(serverExecutable)) {
			destroyThreadManager();
			this.serverExecutable = serverExecutable;
			createThreadManager();
		}
	}
	
	public void stop() {
		destroyThreadManager();
	}
	
	////////////////////////////
	// Thread manager handling
	
	private void createThreadManager() {
		threadManager = new ScionThreadManager(serverExecutable);
	}
	
	private void destroyThreadManager() {
		threadManager.dispose();
		threadManager = null;
	}
	
	////////////////
	// State stuff
	

	//////////////////////
	// Specific commands

	private void checkProtocol() {
		ConnectionInfoCommand command = new ConnectionInfoCommand(threadManager);
		if (command.runSync().isOK()) {
			// TODO
		}
	}
	
	public void backgroundTypecheckFile(String fileName) {
		BackgroundTypecheckFileCommand command = new BackgroundTypecheckFileCommand(threadManager, fileName);
		command.runAsync();
	}
	
	public void loadFile(String fileName) {
		loadedFiles.add(fileName);
		reloadFile(fileName);
	}
	
	public void reloadFile(String fileName) {
		LoadCommand loadCommand = new LoadCommand(threadManager, fileName);
		BackgroundTypecheckFileCommand typecheckCommand = new BackgroundTypecheckFileCommand(threadManager, fileName);
		typecheckCommand.runAsyncAfter(loadCommand);
		loadCommand.runAsync();
	}
	
	public void unloadFile(String fileName) {
		// TODO Scion has no command for unloading yet!
		loadedFiles.remove(fileName);
	}
	
	public String thingAtPoint(Location location) {
      ThingAtPointCommand command = new ThingAtPointCommand(threadManager, location);
      command.runSync();
      if (command.getResult().isOK()) {
    	  return command.getThing();
      } else {
    	  return null;
      }
	}
	
	public Location firstDefinitionLocation(String name) {
		NameDefinitionsCommand command = new NameDefinitionsCommand(threadManager, name);
		command.runSync();
		if (command.getResult().isOK() && command.isFound()) {
			return command.getFirstLocation();
		} else {
			return null;
		}
	}
	
}
