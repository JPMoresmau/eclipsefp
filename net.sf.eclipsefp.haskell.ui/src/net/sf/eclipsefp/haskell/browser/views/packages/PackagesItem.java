/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.packages;

import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;

/**
 * Elements that are shown in the packages view.
 * Each package also includes the database where it belongs to,
 * so the parent element in the tree can be recovered.
 * @author Alejandro Serrano
 *
 */
public class PackagesItem {
	private final DatabaseType ty;
	private final HaskellPackage pkg;

	public PackagesItem(final DatabaseType ty, final HaskellPackage pkg) {
		this.ty = ty;
		this.pkg = pkg;
	}

	public DatabaseType getDatabase() {
		return this.ty;
	}

	public HaskellPackage getPackage() {
		return this.pkg;
	}
}
