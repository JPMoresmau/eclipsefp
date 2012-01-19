/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.packages;

import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;

/**
 * Elements that are shown in the packages view.
 * Each package also includes the database where it belongs to,
 * so the parent element in the tree can be recovered.
 * @author Alejandro Serrano
 *
 */
public class PackagesItem {
	private final Database db;
	private final HaskellPackage pkg;

	public PackagesItem(final Database db, final HaskellPackage pkg) {
		this.db = db;
		this.pkg = pkg;
	}

	public Database getDatabase() {
		return this.db;
	}

	public HaskellPackage getPackage() {
		return this.pkg;
	}
}
