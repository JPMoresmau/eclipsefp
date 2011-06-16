package net.sf.eclipsefp.haskell.browser.client;

import java.io.IOException;

import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.Module;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.Packaged;

import org.json.JSONException;

/**
 * A virtual connection to a server which gives no responses.
 * Used at initialization time and when there is no scion-browser installed.
 * 
 * @author serras
 */

public class NullBrowserServer extends BrowserServer {

	public NullBrowserServer() {
		// Do nothing
	}

	@Override
	public void loadLocalDatabase(String path, boolean rebuild) throws IOException, JSONException {
		// Do nothing
	}

	@Override
	public void setCurrentDatabase(CurrentDatabase current, PackageIdentifier id)
			throws IOException, JSONException {
		// Do nothing
	}

	@Override
	public HaskellPackage[] getPackages() throws IOException, JSONException {
		// Return nothing
		return new HaskellPackage[0];
	}

	@Override
	public Module[] getAllModules() throws IOException, JSONException {
		// Return nothing
		return new Module[0];
	}

	@Override
	public Module[] getModules(String module) throws IOException, JSONException {
		// Return nothing
		return new Module[0];
	}

	@Override
	public Packaged<Declaration>[] getDeclarations(String module) throws Exception {
		// Return nothing
		return (Packaged<Declaration>[])new Packaged[0];
	}

}
