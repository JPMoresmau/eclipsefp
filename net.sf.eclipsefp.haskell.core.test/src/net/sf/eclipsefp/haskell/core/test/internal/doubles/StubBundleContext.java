package net.sf.eclipsefp.haskell.core.test.internal.doubles;

import java.io.File;
import java.io.InputStream;
import java.util.Dictionary;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;
import org.osgi.framework.BundleListener;
import org.osgi.framework.Filter;
import org.osgi.framework.FrameworkListener;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceListener;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;

public class StubBundleContext implements BundleContext {

	public void addBundleListener(BundleListener listener) {
	}

	public void addFrameworkListener(FrameworkListener listener) {
	}

	public void addServiceListener(ServiceListener listener) {
	}

	public void addServiceListener(ServiceListener listener, String filter)
			throws InvalidSyntaxException {
	}

	public Filter createFilter(String filter) throws InvalidSyntaxException {
		return null;
	}

	public ServiceReference[] getAllServiceReferences(String clazz,
			String filter) throws InvalidSyntaxException {
		return null;
	}

	public Bundle getBundle() {
		return new StubBundle();
	}

	public Bundle getBundle(long id) {
		return null;
	}

	public Bundle[] getBundles() {
		return null;
	}

	public File getDataFile(String filename) {
		return null;
	}

	public String getProperty(String key) {
		return null;
	}

	public Object getService(ServiceReference reference) {
		return null;
	}

	public ServiceReference getServiceReference(String clazz) {
		return null;
	}

	public ServiceReference[] getServiceReferences(String clazz, String filter)
			throws InvalidSyntaxException {
		return null;
	}

	public Bundle installBundle(String location) throws BundleException {
		return null;
	}

	public Bundle installBundle(String location, InputStream input)
			throws BundleException {
		return null;
	}

	public ServiceRegistration registerService(String[] clazzes,
			Object service, Dictionary properties) {
		return null;
	}

	public ServiceRegistration registerService(String clazz, Object service,
			Dictionary properties) {
		return null;
	}

	public void removeBundleListener(BundleListener listener) {
	}

	public void removeFrameworkListener(FrameworkListener listener) {
	}

	public void removeServiceListener(ServiceListener listener) {
	}

	public boolean ungetService(ServiceReference reference) {
		return false;
	}

}
