package net.sf.eclipsefp.haskell.browser;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;

import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.json.JSONException;

/**
 * Abstract class for communicating with a scion-browser instance.
 * 
 * @author serras
 */
public abstract class BrowserServer {

	protected Writer logStream = null;
	protected ArrayList<IDatabaseLoadedListener> dbLoadedListeners = new ArrayList<IDatabaseLoadedListener>();

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
	
	protected void notifyDatabaseLoaded(DatabaseLoadedEvent e) {
		for (IDatabaseLoadedListener listener : dbLoadedListeners)
			listener.databaseLoaded(e);
	}

	public abstract void loadLocalDatabase(String path, boolean rebuild) throws IOException,
			JSONException;

	public abstract void setCurrentDatabase(DatabaseType current, PackageIdentifier id)
			throws IOException, JSONException;

	public abstract HaskellPackage[] getPackages() throws IOException, JSONException;

	public abstract Module[] getAllModules() throws IOException, JSONException;

	public abstract Module[] getModules(String module) throws IOException, JSONException;

	public abstract Packaged<Declaration>[] getDeclarations(String module) throws Exception;

	public abstract void stop();
}
