package net.sf.eclipsefp.haskell.browser.views.modules;

import net.sf.eclipsefp.haskell.browser.util.ImageCache;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

public class ModulesLabelProvider implements ILabelProvider {

	public Image getImage(Object element) {
		return ImageCache.MODULE;
	}

	public String getText(Object element) {
		ModulesItem item = (ModulesItem)element;
		return item.getShownName();
	}

	// Listeners: not used
	public void addListener(ILabelProviderListener listener) {
		// Do nothing
	}

	public void dispose() {
		// Do nothing
	}

	public boolean isLabelProperty(Object element, String property) {
		// Do nothing
		return false;
	}

	public void removeListener(ILabelProviderListener listener) {
		// Do nothing
	}
}
