/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.packages;

import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
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
		return element instanceof Database ? ImageCache.DATABASE : ImageCache.PACKAGE;
	}

	public String getText(final Object element) {
		if (element instanceof Database) {
			switch (((Database) element).getType()) {
			case ALL:
				return UITexts.browser_allDatabases;
			case HACKAGE:
				return UITexts.browser_hackageDatabase;
			case LOCAL:
				return UITexts.browser_localDatabase;
			case PACKAGE:
				return UITexts.browser_packageDatabase;
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
