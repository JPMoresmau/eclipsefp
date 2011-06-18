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
	public static Image DATATYPE = BrowserPlugin.getImageDescriptor("icons/obj16/datadecl.gif")
			.createImage();
	public static Image CONSTRUCTOR = BrowserPlugin.getImageDescriptor(
			"icons/obj16/constructordecl.gif").createImage();
	public static Image CLASS = BrowserPlugin.getImageDescriptor("icons/obj16/classdecl.gif")
			.createImage();
	public static Image INSTANCE = BrowserPlugin.getImageDescriptor("icons/obj16/instancedecl.gif")
			.createImage();
	public static Image FUNCTION = BrowserPlugin.getImageDescriptor(
			"icons/obj16/functionbinding.gif").createImage();
	public static Image TYPE = BrowserPlugin.getImageDescriptor("icons/obj16/typedecl.gif")
			.createImage();
}
