/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.packages;

import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;

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
