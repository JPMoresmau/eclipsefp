/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.modules;

import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

/**
 * Label provider for modules view.
 * @author Alejandro Serrano
 *
 */
public class ModulesLabelProvider implements ILabelProvider {

	@Override
  public Image getImage(final Object element) {
		return ImageCache.MODULE;
	}

	@Override
  public String getText(final Object element) {
		ModulesItem item = (ModulesItem)element;
		return item.getShownName();
	}

	// Listeners: not used
	@Override
  public void addListener(final ILabelProviderListener listener) {
		// Do nothing
	}

	@Override
  public void dispose() {
		// Do nothing
	}

	@Override
  public boolean isLabelProperty(final Object element, final String property) {
		// Do nothing
		return false;
	}

	@Override
  public void removeListener(final ILabelProviderListener listener) {
		// Do nothing
	}
}
