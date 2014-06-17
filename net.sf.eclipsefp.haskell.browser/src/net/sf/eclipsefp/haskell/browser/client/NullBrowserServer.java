/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.client;

import java.io.IOException;

import net.sf.eclipsefp.haskell.browser.BrowserServer;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.DeclarationId;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.HoogleStatus;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.json.JSONException;

/**
 * A virtual connection to a server which gives no responses. Used at
 * initialization time and when there is no scion-browser installed.
 * 
 * @author Alejandro Serrano
 */
public class NullBrowserServer extends BrowserServer {

	public NullBrowserServer() {
		// Do nothing
	}
	
	@Override
	public boolean isLocalDatabaseLoaded() {
		return true;
	}
	
	@Override
	public boolean isHackageDatabaseLoaded() {
		return true;
	}
	
	@Override
	public boolean isHoogleLoaded() {
		return true;
	}

	@Override
	protected void loadLocalDatabaseInternal(String path, boolean rebuild) throws IOException, JSONException {
		// Do nothing
	}
	
	@Override
	protected void loadHackageDatabaseInternal(String path, boolean rebuild) throws IOException, JSONException {
		// Do nothing
	}

//	@Override
//	public void setCurrentDatabase(DatabaseType current, PackageIdentifier id) throws IOException,
//			JSONException {
//		// Do nothing
//	}

	@Override
	public HaskellPackage[] getPackages(Database db) throws IOException, JSONException {
		// Return nothing
		return new HaskellPackage[0];
	}

	@Override
	public Module[] getAllModules(Database db) throws IOException, JSONException {
		// Return nothing
		return new Module[0];
	}

	@Override
	public Module[] getModules(Database db,String module) throws IOException, JSONException {
		// Return nothing
		return new Module[0];
	}

	@Override
	public Packaged<Declaration>[] getDeclarations(Database db,String module) throws Exception {
		// Return nothing
		@SuppressWarnings("unchecked")
		Packaged<Declaration>[] result = new Packaged[0];
		return result;
	}
	
	/* (non-Javadoc)
	 * @see net.sf.eclipsefp.haskell.browser.BrowserServer#getDeclarationsFromPrefix(net.sf.eclipsefp.haskell.browser.Database, java.lang.String)
	 */
	@Override
	public Packaged<Declaration>[] getDeclarationsFromPrefix(Database db,
			String prefix) throws Exception {
		@SuppressWarnings("unchecked")
		Packaged<Declaration>[] result = new Packaged[0];
		return result;
	}
	
	@Override
	public DeclarationId[] findModulesForDeclaration(Database db,String decl) throws IOException, JSONException {
		return new DeclarationId[0];
	}
	
	@Override
	public void setExtraHooglePath(String newPath) throws IOException, JSONException {
		// Do nothing
	}

	@Override
	public HoogleResult[] queryHoogle(Database db,String query) throws Exception {
		// Return nothing
		return new HoogleResult[0];
	}
	
	@Override
	public void downloadHoogleData() {
		// Do nothing
	}
	
	@Override
	public HoogleStatus checkHoogle() {
		return HoogleStatus.MISSING;
	}

	@Override
	public void stop() {
		// Do nothing
	}
}
