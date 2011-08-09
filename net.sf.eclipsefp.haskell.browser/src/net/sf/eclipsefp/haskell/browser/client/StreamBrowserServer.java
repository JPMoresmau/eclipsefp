/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.client;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

import net.sf.eclipsefp.haskell.browser.BrowserEvent;
import net.sf.eclipsefp.haskell.browser.BrowserServer;
import net.sf.eclipsefp.haskell.browser.DatabaseLoadedEvent;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.eclipse.core.runtime.IPath;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Class used for communicating with a Scion Browser instance.
 * 
 * @author serras
 */

public class StreamBrowserServer extends BrowserServer {

	private IPath serverExecutable;
	private Process process = null;
	private BufferedWriter in = null;
	private BufferedReader out = null;
	private boolean dbLoaded = false;
	private boolean hoogleLoaded = false;

	public StreamBrowserServer(IPath serverExecutable) throws Exception {
		this.serverExecutable = serverExecutable;
		startServer();
	}

	public void startServer() throws Exception {
		ProcessBuilder builder = new ProcessBuilder(
				serverExecutable.toOSString());
		builder.redirectErrorStream(true);

		try {
			process = builder.start();
			out = new BufferedReader(new InputStreamReader(
					process.getInputStream(), "UTF8"));
			in = new BufferedWriter(new OutputStreamWriter(
					process.getOutputStream(), "UTF8"));
		} catch (Throwable ex) {
			throw new Exception("Could not load");
		}
	}

	public synchronized String sendAndReceive(JSONObject input)
			throws IOException {
		String jsonInput = input.toString();
		log(">> " + jsonInput);
		in.write(jsonInput + "\n");
		in.flush();
		String response = out.readLine();
		// log(response);
		return response;
	}

	public synchronized void sendAndReceiveOk(JSONObject input)
			throws IOException {
		String jsonInput = input.toString();
		log(">> " + jsonInput);
		in.write(jsonInput + "\n");
		in.flush();

		String response = null;
		do {
			response = out.readLine();
			log(response);
		} while (!response.equals("\"ok\""));
	}
	
	@Override
	public boolean isDatabaseLoaded() {
		return dbLoaded;
	}
	
	@Override
	public boolean isHoogleLoaded() {
		return hoogleLoaded;
	}

	@Override
	public void loadLocalDatabase(String path, boolean rebuild)
			throws IOException, JSONException {
		sendAndReceiveOk(Commands.createLoadLocalDatabase(path, rebuild));
		// Notify listeners
		DatabaseLoadedEvent e = new DatabaseLoadedEvent(this, path,
				DatabaseType.LOCAL);
		dbLoaded = true;
		notifyDatabaseLoaded(e);
	}

	@Override
	public void setCurrentDatabase(DatabaseType current, PackageIdentifier id)
			throws IOException, JSONException {
		sendAndReceiveOk(Commands.createSetCurrentDatabase(current, id));
	}

	@Override
	public HaskellPackage[] getPackages() throws IOException, JSONException {
		String response = sendAndReceive(Commands.createGetPackages());
		return Commands.responseGetPackages(response);
	}

	@Override
	public Module[] getAllModules() throws IOException, JSONException {
		String response = sendAndReceive(Commands.createGetAllModules());
		return Commands.responseGetModules(response);
	}

	@Override
	public Module[] getModules(String module) throws IOException, JSONException {
		String response = sendAndReceive(Commands.createGetModules(module));
		return Commands.responseGetModules(response);
	}

	@Override
	public Packaged<Declaration>[] getDeclarations(String module)
			throws Exception {
		String response = sendAndReceive(Commands.createGetDeclarations(module));
		return Commands.responseGetDeclarations(response);
	}

	@Override
	public HoogleResult[] queryHoogle(String query) throws Exception {
		String response = sendAndReceive(Commands.createHoogleQuery(query));
		return Commands.responseHoogleQuery(response);
	}

	@Override
	public void downloadHoogleData() throws IOException, JSONException {
		sendAndReceiveOk(Commands.createDownloadHoogleData());
	}

	@Override
	public boolean checkHoogle() throws Exception {
		this.setCurrentDatabase(DatabaseType.ALL, null);
		// We know that "fmap" is always present
		HoogleResult[] mapResults = this.queryHoogle("fmap");
		boolean isPresent = mapResults.length > 0;
		// If is present, notify the views
		if (isPresent) {
			hoogleLoaded = true;
			notifyHoogleLoaded(new BrowserEvent(this));
		}
		return isPresent;
	}

	@Override
	public void stop() {
		if (process != null) {
			// Nothing is loaded
			dbLoaded = false;
			hoogleLoaded = false;
			// Tell we no longer have a database
			BrowserEvent e = new BrowserEvent(this);
			notifyDatabaseUnloaded(e);
			// Nor a Hoogle connection
			notifyHoogleUnloaded(e);
			// Close connection with the process
			process.destroy();
		}
	}
}
