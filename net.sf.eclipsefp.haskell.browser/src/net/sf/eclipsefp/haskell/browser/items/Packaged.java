/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.items;

public class Packaged<Element> {
	PackageIdentifier pkg;
	Element elt;
	
	public Packaged(PackageIdentifier pkg, Element elt) {
		this.pkg = pkg;
		this.elt = elt;
	}
	
	public PackageIdentifier getPackage() {
		return this.pkg;
	}
	
	public Element getElement() {
		return this.elt;
	}
}
