/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser;

/**
 * Information about the event of loading a database in scion-browser.
 * 
 * @author Alejandro Serrano
 */
public class DatabaseLoadedEvent extends BrowserEvent {
	String db_path;
	DatabaseType ty;

	public DatabaseLoadedEvent(BrowserServer server, String db_path, DatabaseType ty) {
		super(server);
		this.db_path = db_path;
		this.ty = ty;
	}
	
	/**
	 * Returns the path to the database being loaded
	 * 
	 * @return
	 */
	public String getDatabasePath() {
		return this.db_path;
	}
	
	/**
	 * Returns the type of database being loaded:
	 * * LOCAL for a database of locally installed packages
	 * * HACKAGE for the Hackage database
	 * * PACKAGE for a alone package database
	 * 
	 * @return
	 */
	public DatabaseType getType() {
		return this.ty;
	}
}
