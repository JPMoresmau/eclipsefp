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
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.zip.InflaterInputStream;

import net.sf.eclipsefp.haskell.browser.BrowserEvent;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.BrowserServer;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.DatabaseLoadedEvent;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.DeclarationId;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.HoogleStatus;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.Packaged;
import net.sf.eclipsefp.haskell.browser.util.BrowserText;
import net.sf.eclipsefp.haskell.util.CappedStringWriter;
import net.sf.eclipsefp.haskell.util.DispatchWriter;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.LangUtil;
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
	public LockObject lock= new LockObject();
	
	/**
	 * cache packages by database
	 */
	private Map<Database,HaskellPackage[]> packageCache=new HashMap<Database,HaskellPackage[]>();
	
	//private DatabaseType currentDatabase;
	private HashMap<String, Packaged<Declaration>[]> declCache = new HashMap<String, Packaged<Declaration>[]>();

	private boolean logError;
	
	private CappedStringWriter lastErrW=new CappedStringWriter(10000);
	private DispatchWriter allErrWs=new DispatchWriter();
	
	public StreamBrowserServer(IPath serverExecutable,boolean logError) throws Exception {
		this.serverExecutable = serverExecutable;
		this.logError=logError;
		startServer(logError);
	}

	@Override
	public void setLogStream(Writer logStream) {
		super.setLogStream(logStream);
		if (logError){
			allErrWs.getWriters().clear();
			allErrWs.getWriters().add(lastErrW);
			if (logError && logStream!=null){
				allErrWs.getWriters().add(logStream);
			}
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
			allErrWs.getWriters().clear();
			allErrWs.getWriters().add(lastErrW);
			if (logError && logStream!=null){
				allErrWs.getWriters().add(logStream);
			}
			errorRedirect=new StreamRedirect(new InputStreamReader(process.getErrorStream(),FileUtil.UTF8), allErrWs);
			errorRedirect.start();
			/*out = new BufferedReader(new InputStreamReader(
					process.getInputStream(), FileUtil.UTF8));*/
			/*err = new BufferedReader(new InputStreamReader(
					process.getErrorStream(), FileUtil.UTF8)); */
			in  = new BufferedWriter(new OutputStreamWriter(
					process.getOutputStream(),FileUtil.UTF8));
		} catch (Throwable ex) {
			throw new Exception("Could not load");
		}
	}

	public String sendAndReceive(JSONObject input)
			throws IOException {
		synchronized(lock) {
			lock.setRunning(true);
			try {
				String jsonInput = input.toString();
				sendCommand(jsonInput);
				String response=getALine();
				// String response = out.readLine();
				//log(response);
				if (response==null){
					response=new JSONArray().toString();
				}
				return response;
			} finally {
				lock.setRunning(false);
			}
		}
	}

	public boolean sendAndReceiveOk(JSONObject input)
			throws IOException {
		synchronized(lock) {
			lock.setRunning(true);
			try {
				String jsonInput = input.toString();
				sendCommand(jsonInput);
		
				String response = null;
				do {
					response = getALine(); // out.readLine();
					log(response);
				} while (response!=null && !response.equals("\"ok\""));
				return "\"ok\"".equals(response);
			} finally {
				lock.setRunning(false);
			}
		}
	}
	
	public boolean sendAndReceiveBoolean(JSONObject input)
			throws IOException {
		synchronized(lock) {
			lock.setRunning(true);
			try {
		
				String jsonInput = input.toString();
				sendCommand(jsonInput);
		
				String response = null;
				do {
					response = getALine(); // out.readLine();
					log(response);
				} while (response!=null && !response.equals("true") && !response.equals("false"));
				
				return "true".equals(response);
			} finally {
				lock.setRunning(false);
			}
		}
	}
	
	private void sendCommand(String command) throws IOException{
		log(">> " + command);
		lastErrW.clear();
		in.write(command + "\n");
		in.flush();
	}
	
	public HoogleStatus sendAndReceiveStatus(JSONObject input)
			throws IOException {
		synchronized(lock) {
			lock.setRunning(true);
			try {
				String jsonInput = input.toString();
				sendCommand(jsonInput);
				HoogleStatus st=null;
				String response = null;
				do {
					response = getALine(); // out.readLine();
					log(response);
					try {
						st=HoogleStatus.valueOf(LangUtil.unquote(response).toUpperCase());
					} catch (IllegalArgumentException iae){
						// NOOP
					}
				} while (st==null);
				
				return st;
			} finally {
				lock.setRunning(false);
			}
		}
	}
	
	public String getALine() throws IOException{
		try {			
			InflaterInputStream gzip = new InflaterInputStream(out);
			BufferedReader reader = new BufferedReader(
					new InputStreamReader(gzip, FileUtil.UTF8));
			
			return reader.readLine();
		} catch (IOException e) {
			BrowserPlugin.logError(BrowserText.error_read, e);
			String lastErr=lastErrW.toString().trim();
			if (lastErr!=null && lastErr.length()>0){
				throw new IOException(lastErr);
			}
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
			packageCache.remove(Database.LOCAL);
			packageCache.remove(Database.ALL);
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
			packageCache.remove(Database.HACKAGE);
			packageCache.remove(Database.ALL);
			// Notify listeners
			DatabaseLoadedEvent e = new DatabaseLoadedEvent(this, path,
					DatabaseType.HACKAGE);
			hackageDbLoaded = true;
			notifyDatabaseLoaded(e);
		}
	}

//	@Override
//	public void setCurrentDatabase(DatabaseType current, PackageIdentifier id)
//			throws IOException, JSONException {
//		if (this.currentDatabase==null || !this.currentDatabase.equals(current) || id!=null){
//			this.currentDatabase = current;
//			sendAndReceiveOk(Commands.createSetCurrentDatabase(current, id));
//		}
//	}


	
	@Override
	public HaskellPackage[] getPackages(Database db) throws IOException, JSONException {
		HaskellPackage[] ret=packageCache.get(db);
		if (ret==null){
			String response = sendAndReceive(Commands.createGetPackages(db));
			ret= Commands.responseGetPackages(response);
			packageCache.put(db, ret);
		}
		return ret;
	}

	@Override
	public Module[] getAllModules(Database db) throws IOException, JSONException {
		String response = sendAndReceive(Commands.createGetAllModules(db));
		return Commands.responseGetModules(response);
	}

	@Override
	public Module[] getModules(Database db,String module) throws IOException, JSONException {
		String response = sendAndReceive(Commands.createGetModules(db,module));
		return Commands.responseGetModules(response);
	}

	@SuppressWarnings("unchecked")
	@Override
	public Packaged<Declaration>[] getDeclarations(Database db,String module)
			throws Exception {
		// Try to find in cache
		//if (this.currentDatabase == DatabaseType.ALL) {
		Packaged<Declaration>[] decls=this.declCache.get(module);
		if (decls!=null){
			return decls;
		}
		//}
		// If not, search
		String response = sendAndReceive(Commands.createGetDeclarations(db,module));
		decls = Commands.responseGetDeclarations(response);
		if (decls==null){
			decls=new Packaged[0];
		}
		// Check if we need to save in cache
		//if (this.currentDatabase == DatabaseType.ALL) {
		this.declCache.put(module, decls);
		//}
		return decls;
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Packaged<Declaration>[] getDeclarationsFromPrefix(Database db,String prefix)
			throws Exception {

		String response = sendAndReceive(Commands.createGetDeclarationsFromPrefix(db, prefix));
		Packaged<Declaration>[] decls = Commands.responseGetDeclarationsFromPrefix(response);
		if (decls==null){
			decls=new Packaged[0];
		}

		return decls;
	}
	
	@Override
	public DeclarationId[] findModulesForDeclaration(Database db,String decl) throws IOException, JSONException {
		String response = sendAndReceive(Commands.createFindModulesForDeclaration(db,decl));
		return Commands.responseGetDeclarationId(response);
	}
	
	@Override
	public void setExtraHooglePath(String newPath) throws IOException, JSONException {
		sendAndReceiveOk(Commands.createSetExtraHooglePath(newPath));
	}

	@Override
	public HoogleResult[] queryHoogle(Database db,String query) throws Exception {
		String response = sendAndReceive(Commands.createHoogleQuery(db,query));
		return Commands.responseHoogleQuery(response);
	}

	@Override
	public void downloadHoogleData() throws IOException, JSONException {
		sendAndReceiveStatus(Commands.createDownloadHoogleData());
	}

	@Override
	public HoogleStatus checkHoogle() throws Exception {
		HoogleStatus st = sendAndReceiveStatus(Commands.createCheckHoogleData());
		// If is present, notify the views
		if (HoogleStatus.OK.equals(st)) {
			hoogleLoaded = true;
			notifyHoogleLoaded(new BrowserEvent(this));
		}
		return st;
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
	
	public boolean isRunning(){
		return lock.isRunning();
	}
	
	private static class LockObject{
		AtomicBoolean running=new AtomicBoolean(false);

		public boolean isRunning() {
			return running.get();
		}

		public void setRunning(boolean running) {
			this.running.set(running);
		}
		
		
	}
}
