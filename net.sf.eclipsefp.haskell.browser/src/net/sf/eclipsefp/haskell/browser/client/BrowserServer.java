package net.sf.eclipsefp.haskell.browser.client;

import java.io.IOException;
import java.io.Writer;

import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.json.JSONException;

public abstract class BrowserServer {

	protected Writer logStream = null;

	public void setLogStream(final Writer logStream) {
		this.logStream = logStream;
	}

	protected void log(String msg) {
		try {
			if (logStream != null)
				logStream.write(msg);
		} catch (Throwable ex) {

		}
	}

	public abstract void loadLocalDatabase(String path, boolean rebuild)
			throws IOException, JSONException;

	public abstract void setCurrentDatabase(CurrentDatabase current,
			PackageIdentifier id) throws IOException, JSONException;

	public abstract HaskellPackage[] getPackages() throws IOException,
			JSONException;

	public abstract Module[] getAllModules() throws IOException, JSONException;

	public abstract Module[] getModules(String module) throws IOException,
			JSONException;

	public abstract Packaged<Declaration>[] getDeclarations(String module)
			throws Exception;
}
