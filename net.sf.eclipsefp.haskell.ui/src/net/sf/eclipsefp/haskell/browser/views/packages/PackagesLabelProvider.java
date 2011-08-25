/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.packages;

import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

/**
 * Label provider for packages.
 * @author Alejandro Serrano
 *
 */
public class PackagesLabelProvider implements ILabelProvider {

	public Image getImage(final Object element) {
		return element instanceof DatabaseType ? ImageCache.DATABASE : ImageCache.PACKAGE;
	}

	public String getText(final Object element) {
		if (element instanceof DatabaseType) {
			switch ((DatabaseType) element) {
			case ALL:
				return "All";
			case HACKAGE:
				return "Hackage";
			case LOCAL:
				return "Local";
			case PACKAGE:
				return "Package";
			}
			return "";
		} else {
			PackagesItem pkg = (PackagesItem)element;
			return pkg.getPackage().getIdentifier().toString();
		}
	}

	// Listeners: not used
	public void addListener(final ILabelProviderListener listener) {
		// Do nothing
	}

	public void dispose() {
		// Do nothing
	}

	public boolean isLabelProperty(final Object element, final String property) {
		// Do nothing
		return false;
	}

	public void removeListener(final ILabelProviderListener listener) {
		// Do nothing
	}
}
