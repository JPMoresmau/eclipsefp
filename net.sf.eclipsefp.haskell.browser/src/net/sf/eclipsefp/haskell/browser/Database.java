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
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((pkgId == null) ? 0 : pkgId.hashCode());
		result = prime * result + ((type == null) ? 0 : type.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Database other = (Database) obj;
		if (pkgId == null) {
			if (other.pkgId != null)
				return false;
		} else if (!pkgId.equals(other.pkgId))
			return false;
		if (type != other.type)
			return false;
		return true;
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
