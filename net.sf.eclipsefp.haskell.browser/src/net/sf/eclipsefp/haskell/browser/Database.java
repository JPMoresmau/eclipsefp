/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.browser;

import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;

/**
 * Encapsulates DatabaseType + PkgIdentifier, peer of CurrentDatabase in PersistentCommands.hs
 * @author JP Moresmau
 *
 */
public class Database {
	private DatabaseType type;
	private PackageIdentifier pkgId;
	
	public static Database ALL=new Database(DatabaseType.ALL,null);
	public static Database LOCAL=new Database(DatabaseType.LOCAL,null);
	public static Database HACKAGE=new Database(DatabaseType.HACKAGE,null);
	
	public static Database Package(PackageIdentifier pkgId){
		if (pkgId==null){
			throw new IllegalArgumentException("pkgId==null");
		}
		return new Database(DatabaseType.PACKAGE,pkgId);
	}
	
	/**
	 * private, use static methods to get proper instances
	 * @param type
	 * @param pkgId
	 */
	private Database(DatabaseType type, PackageIdentifier pkgId) {
		super();
		this.type = type;
		this.pkgId = pkgId;
	}
	
	public DatabaseType getType() {
		return type;
	}
	public PackageIdentifier getPkgId() {
		return pkgId;
	}
	
	
}
