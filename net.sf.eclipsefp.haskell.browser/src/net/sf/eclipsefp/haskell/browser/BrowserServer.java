/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.DeclarationId;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.json.JSONException;

/**
 * Abstract class for communicating with a scion-browser instance.
 * 
 * @author Alejandro Serrano
 */
public abstract class BrowserServer {

	protected Writer logStream = null;
	protected ArrayList<IDatabaseLoadedListener> dbLoadedListeners = new ArrayList<IDatabaseLoadedListener>();
	protected ArrayList<IHoogleLoadedListener> hoogleLoadedListeners = new ArrayList<IHoogleLoadedListener>();
	
	protected HashMap<String, Module> moduleDocs = new HashMap<String, Module>();

	/**
	 * Sets the stream where log messages will be sent
	 * 
	 * @param logStream
	 *            the new log stream
	 */
	public void setLogStream(Writer logStream) {
		this.logStream = logStream;
	}

	/**
	 * Logs a message, usually into an Eclipse console
	 * 
	 * @param msg
	 *            string to be shown
	 */
	protected void log(String msg) {
		try {
			if (logStream != null) {
				logStream.write(msg + "\n");
				logStream.flush();
			}
		} catch (Throwable ex) {

		}
	}

	public void addDatabaseLoadedListener(IDatabaseLoadedListener listener) {
		dbLoadedListeners.add(listener);
	}

	public void addHoogleLoadedListener(IHoogleLoadedListener listener) {
		hoogleLoadedListeners.add(listener);
	}

	protected void notifyDatabaseLoaded(DatabaseLoadedEvent e) {
		for (IDatabaseLoadedListener listener : dbLoadedListeners)
			listener.databaseLoaded(e);
	}

	protected void notifyDatabaseUnloaded(BrowserEvent e) {
		for (IDatabaseLoadedListener listener : dbLoadedListeners)
			listener.databaseUnloaded(e);
	}

	protected void notifyHoogleLoaded(BrowserEvent e) {
		for (IHoogleLoadedListener listener : hoogleLoadedListeners)
			listener.hoogleLoaded(e);
	}

	protected void notifyHoogleUnloaded(BrowserEvent e) {
		for (IHoogleLoadedListener listener : hoogleLoadedListeners)
			listener.hoogleUnloaded(e);
	}

	public void loadLocalDatabase(String path, boolean rebuild) throws IOException, JSONException {
		loadLocalDatabaseInternal(path, rebuild);
		// Cache information of all the modules
		//this.setCurrentDatabase(DatabaseType.ALL, null);
		for (Module m : this.getAllModules(Database.ALL)) {
			moduleDocs.put(m.getName(), m);
		}
	}
	
	public void loadHackageDatabase(String path, boolean rebuild) throws IOException, JSONException {
		loadHackageDatabaseInternal(path, rebuild);
		// Cache information of all the modules
		/*this.setCurrentDatabase(DatabaseType.ALL, null);
		for (Module m : this.getAllModules()) {
			moduleDocs.put(m.getName(), m);
		}*/
	}
	
	public Module getCachedModule(String module) {
		return moduleDocs.get(module);
	}
	
	public Set<String> getCachedModuleNames() {
		return moduleDocs.keySet();
	}
	
	public boolean isAnyDatabaseLoaded() {
		return isLocalDatabaseLoaded() || isHackageDatabaseLoaded();
	}

	public abstract boolean isLocalDatabaseLoaded();
	
	public abstract boolean isHackageDatabaseLoaded();

	public abstract boolean isHoogleLoaded();

	protected abstract void loadLocalDatabaseInternal(String path, boolean rebuild) throws IOException, JSONException;
	
	protected abstract void loadHackageDatabaseInternal(String path, boolean rebuild) throws IOException, JSONException;

//	public abstract void setCurrentDatabase(DatabaseType current, PackageIdentifier id) throws IOException,
//			JSONException;

	public abstract HaskellPackage[] getPackages(Database db) throws IOException, JSONException;

	public abstract Module[] getAllModules(Database db) throws IOException, JSONException;

	public abstract Module[] getModules(Database db,String module) throws IOException, JSONException;

	public abstract Packaged<Declaration>[] getDeclarations(Database db,String module) throws Exception;
	
	public abstract Packaged<Declaration>[] getDeclarationsFromPrefix(Database db,String prefix) throws Exception ;
	
	public abstract DeclarationId[] findModulesForDeclaration(Database db,String decl) throws IOException, JSONException;
	
	public abstract void setExtraHooglePath(String newPath) throws IOException, JSONException;

	public abstract HoogleResult[] queryHoogle(Database db,String query) throws Exception;

	public abstract void downloadHoogleData() throws IOException, JSONException;

	public abstract boolean checkHoogle() throws Exception;

	public abstract void stop();
}
