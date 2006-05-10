package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Dictionary;
import java.util.Enumeration;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleException;
import org.osgi.framework.ServiceReference;

public class StubBundle implements Bundle {

	public Enumeration findEntries(String path, String filePattern,
			boolean recurse) {
		return null;
	}

	public long getBundleId() {
		return 0;
	}

	public URL getEntry(String name) {
		return null;
	}

	public Enumeration getEntryPaths(String path) {
		return null;
	}

	public Dictionary getHeaders() {
		return null;
	}

	public Dictionary getHeaders(String locale) {
		return null;
	}

	public long getLastModified() {
		return 0;
	}

	public String getLocation() {
		return null;
	}

	public ServiceReference[] getRegisteredServices() {
		return null;
	}

	public URL getResource(String name) {
		return null;
	}

	public Enumeration getResources(String name) throws IOException {
		return null;
	}

	public ServiceReference[] getServicesInUse() {
		return null;
	}

	public int getState() {
		return 0;
	}

	public String getSymbolicName() {
		return "net.sf.eclipsefp.haskell.core.test.internal.StubBundle";
	}

	public boolean hasPermission(Object permission) {
		// TODO Auto-generated method stub
		return false;
	}

	public Class loadClass(String name) throws ClassNotFoundException {
		// TODO Auto-generated method stub
		return null;
	}

	public void start() throws BundleException {
		// TODO Auto-generated method stub

	}

	public void stop() throws BundleException {
		// TODO Auto-generated method stub

	}

	public void uninstall() throws BundleException {
		// TODO Auto-generated method stub

	}

	public void update() throws BundleException {
		// TODO Auto-generated method stub

	}

	public void update(InputStream in) throws BundleException {
		// TODO Auto-generated method stub

	}

}
