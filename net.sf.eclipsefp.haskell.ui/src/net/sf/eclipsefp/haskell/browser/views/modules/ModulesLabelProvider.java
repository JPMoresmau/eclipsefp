/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.modules;

import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

public class ModulesLabelProvider implements ILabelProvider {

	public Image getImage(final Object element) {
		return ImageCache.MODULE;
	}

	public String getText(final Object element) {
		ModulesItem item = (ModulesItem)element;
		return item.getShownName();
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
