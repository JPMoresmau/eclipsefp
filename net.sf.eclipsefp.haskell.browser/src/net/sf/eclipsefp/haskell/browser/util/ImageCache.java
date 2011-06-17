package net.sf.eclipsefp.haskell.browser.util;

import org.eclipse.swt.graphics.Image;

import net.sf.eclipsefp.haskell.browser.BrowserPlugin;

public class ImageCache {

	public static Image MODULE = BrowserPlugin.getImageDescriptor("icons/obj16/module.gif")
			.createImage();
	public static Image PACKAGE = BrowserPlugin.getImageDescriptor("icons/obj16/package.gif")
			.createImage();
	public static Image DATABASE = BrowserPlugin
			.getImageDescriptor("icons/obj16/packagefolder.gif").createImage();
}
