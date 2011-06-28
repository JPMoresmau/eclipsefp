/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.declarations;

import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.Declaration;
import net.sf.eclipsefp.haskell.browser.items.QueryItem;
import net.sf.eclipsefp.haskell.browser.util.ImageCache;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

public class DeclarationsLabelProvider implements ILabelProvider {

	public Image getImage(final Object element) {
		if (element instanceof QueryItem) {
			QueryItem item = (QueryItem) element;
			return ImageCache.getImageForDeclaration( item.getType() );
		} else if (element instanceof Constructor) {
			return ImageCache.CONSTRUCTOR;
		}

		return null;
	}

	public String getText(final Object element) {
		if (element instanceof QueryItem) {
			Declaration elt = ((QueryItem) element).getDeclaration();
			return elt.getShownName();
		} else if (element instanceof Constructor) {
			Constructor item = (Constructor) element;
			return item.getShownName();
		}

		return null;
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
