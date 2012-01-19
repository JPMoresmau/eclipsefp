/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.client;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.zip.InflaterInputStream;

import net.sf.eclipsefp.haskell.browser.BrowserEvent;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.BrowserServer;
import net.sf.eclipsefp.haskell.browser.DatabaseLoadedEvent;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.Packaged;
import net.sf.eclipsefp.haskell.browser.util.BrowserText;
import net.sf.eclipsefp.haskell.util.NullWriter;
import net.sf.eclipsefp.haskell.util.StreamRedirect;

import org.eclipse.core.runtime.IPath;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Class used for communicating with a Scion Browser instance.
 * 
 * @author Alejandro Serrano
 */
public class StreamBrowserServer extends BrowserServer {

	private IPath serverExecutable;
	private Process process = null;
	private BufferedWriter in = null;
	// private BufferedReader err = null;
	private InputStream out = null;
	private boolean localDbLoaded = false;
	private boolean hackageDbLoaded = false;
	private boolean hoogleLoaded = false;
	private StreamRedirect errorRedirect;
	public Object lock;
	
	private DatabaseType currentDatabase;
	private HashMap<String, Packaged<Declaration>[]> declCache;

	private boolean logError;
	
	public StreamBrowserServer(IPath serverExecutable,boolean logError) throws Exception {
		this.serverExecutable = serverExecutable;
		this.declCache = new HashMap<String, Packaged<Declaration>[]>();
		lock = new Object();
		this.logError=logError;
		startServer(logError);
	}

	@Override
	public void setLogStream(Writer logStream) {
		super.setLogStream(logStream);
		if (logError){
			errorRedirect.setOutput(logStream);
		}
	}
	
	public void setLogError(boolean logError) {
		this.logError = logError;
	}
	
	public void startServer(boolean logError) throws Exception {
		ProcessBuilder builder = new ProcessBuilder(
				serverExecutable.toOSString());
		builder.redirectErrorStream(false);

		try {
			process = builder.start();
			out = process.getInputStream();
			Writer errorWriter=logError && logStream!=null?logStream:new NullWriter();
			errorRedirect=new StreamRedirect(new InputStreamReader(process.getErrorStream(),"UTF8"), errorWriter);
			errorRedirect.start();
			/*out = new BufferedReader(new InputStreamReader(
					process.getInputStream(), "UTF8"));*/
			/*err = new BufferedReader(new InputStreamReader(
					process.getErrorStream(), "UTF8")); */
			in  = new BufferedWriter(new OutputStreamWriter(
					process.getOutputStream(), "UTF8"));
		} catch (Throwable ex) {
			throw new Exception("Could not load");
		}
	}

	public String sendAndReceive(JSONObject input)
			throws IOException {
		synchronized(lock) {
			String jsonInput = input.toString();
			log(">> " + jsonInput);
			in.write(jsonInput + "\n");
			in.flush();
			String response=getALine();
			// String response = out.readLine();
			//log(response);
			if (response==null){
				response=new JSONArray().toString();
			}
			return response;
		}
	}

	public boolean sendAndReceiveOk(JSONObject input)
			throws IOException {
		synchronized(lock) {
			String jsonInput = input.toString();
			log(">> " + jsonInput);
			in.write(jsonInput + "\n");
			in.flush();
	
			String response = null;
			do {
				response = getALine(); // out.readLine();
				log(response);
			} while (response!=null && !response.equals("\"ok\""));
			return "\"ok\"".equals(response);
		}
	}
	
	public synchronized boolean sendAndReceiveBoolean(JSONObject input)
			throws IOException {
		synchronized(lock) {
			String jsonInput = input.toString();
			log(">> " + jsonInput);
			in.write(jsonInput + "\n");
			in.flush();
	
			String response = null;
			do {
				response = getALine(); // out.readLine();
				log(response);
			} while (response!=null && !response.equals("true") && !response.equals("false"));
			
			return "true".equals(response);
		}
	}
	
	public String getALine() throws IOException{
		try {			
			InflaterInputStream gzip = new InflaterInputStream(out);
			BufferedReader reader = new BufferedReader(
					new InputStreamReader(gzip, "UTF8"));
			
			return reader.readLine();
		} catch (IOException e) {
			BrowserPlugin.logError(BrowserText.error_read, e);
			throw e;
		}
	}
	
	@Override
	public boolean isLocalDatabaseLoaded() {
		return localDbLoaded;
	}
	
	@Override
	public boolean isHackageDatabaseLoaded() {
		return hackageDbLoaded;
	}
	
	@Override
	public boolean isHoogleLoaded() {
		return hoogleLoaded;
	}

	@Override
	protected void loadLocalDatabaseInternal(String path, boolean rebuild)
			throws IOException, JSONException {
		if (sendAndReceiveOk(Commands.createLoadLocalDatabase(path, rebuild))){
			// Notify listeners
			DatabaseLoadedEvent e = new DatabaseLoadedEvent(this, path,
					DatabaseType.LOCAL);
			localDbLoaded = true;
			notifyDatabaseLoaded(e);
		}
	}
	
	@Override
	protected void loadHackageDatabaseInternal(String path, boolean rebuild)
			throws IOException, JSONException {
		if (sendAndReceiveOk(Commands.createLoadHackageDatabase(path, rebuild))){
			// Notify listeners
			DatabaseLoadedEvent e = new DatabaseLoadedEvent(this, path,
					DatabaseType.HACKAGE);
			hackageDbLoaded = true;
			notifyDatabaseLoaded(e);
		}
	}

	@Override
	public void setCurrentDatabase(DatabaseType current, PackageIdentifier id)
			throws IOException, JSONException {
		if (this.currentDatabase==null || !this.currentDatabase.equals(current) || id!=null){
			this.currentDatabase = current;
			sendAndReceiveOk(Commands.createSetCurrentDatabase(current, id));
		}
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

	@SuppressWarnings("unchecked")
	@Override
	public Packaged<Declaration>[] getDeclarations(String module)
			throws Exception {
		// Try to find in cache
		if (this.currentDatabase == DatabaseType.ALL) {
			Packaged<Declaration>[] decls=this.declCache.get(module);
			if (decls!=null){
				return decls;
			}
		}
		// If not, search
		String response = sendAndReceive(Commands.createGetDeclarations(module));
		Packaged<Declaration>[] decls = Commands.responseGetDeclarations(response);
		if (decls==null){
			decls=new Packaged[0];
		}
		// Check if we need to save in cache
		if (this.currentDatabase == DatabaseType.ALL) {
			this.declCache.put(module, decls);
		}
		return decls;
	}
	
	@Override
	public Module[] findModulesForDeclaration(String decl) throws IOException, JSONException {
		String response = sendAndReceive(Commands.createFindModulesForDeclaration(decl));
		return Commands.responseGetModules(response);
	}
	
	@Override
	public void setExtraHooglePath(String newPath) throws IOException, JSONException {
		sendAndReceiveOk(Commands.createSetExtraHooglePath(newPath));
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
		boolean isPresent = sendAndReceiveBoolean(Commands.createCheckHoogleData());
		// If is present, notify the views
		if (isPresent) {
			hoogleLoaded = true;
			notifyHoogleLoaded(new BrowserEvent(this));
		}
		return isPresent;
	}

	@Override
	public void stop() {
		// Nothing is loaded
		localDbLoaded = false;
		hoogleLoaded = false;
		// Tell we no longer have a database
		BrowserEvent e = new BrowserEvent(this);
		notifyDatabaseUnloaded(e);
		// Nor a Hoogle connection
		notifyHoogleUnloaded(e);
		try {
			if (out!=null){
				out.close();
			}
		} catch (Exception ignore){
			// noop
		}
		try {
			if (in!=null){
				in.close();
			}
		} catch (Exception ignore){
			//noop
		}
		try {
			sendAndReceiveOk(Commands.createQuit());
			if (process!=null){
				process.destroy();
			}
		} catch (Exception ex) {
			if (process!=null){
				process.destroy();
			}
		}
		process = null;
	}
}
