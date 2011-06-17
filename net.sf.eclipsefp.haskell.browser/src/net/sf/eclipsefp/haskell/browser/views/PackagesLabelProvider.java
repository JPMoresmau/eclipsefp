package net.sf.eclipsefp.haskell.browser.views;

import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

public class PackagesLabelProvider implements ILabelProvider {

	public Image getImage(Object element) {
		if (element instanceof DatabaseType)
			return BrowserPlugin.getImageDescriptor("icons/obj16/packagefolder.gif").createImage();
		else
			return BrowserPlugin.getImageDescriptor("icons/obj16/package.gif").createImage();
	}

	public String getText(Object element) {
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
			HaskellPackage pkg = (HaskellPackage)element;
			return pkg.getIdentifier().toString();
		}
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
