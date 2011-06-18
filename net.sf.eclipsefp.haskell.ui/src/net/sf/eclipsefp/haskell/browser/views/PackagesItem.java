package net.sf.eclipsefp.haskell.browser.views;

import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;

public class PackagesItem {
	private DatabaseType ty;
	private HaskellPackage pkg;
	
	public PackagesItem(DatabaseType ty, HaskellPackage pkg) {
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
