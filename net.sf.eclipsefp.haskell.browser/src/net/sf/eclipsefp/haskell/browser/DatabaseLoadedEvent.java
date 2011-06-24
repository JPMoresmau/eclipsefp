/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser;

public class DatabaseLoadedEvent {
	BrowserServer server;
	String db_path;
	DatabaseType ty;

	public DatabaseLoadedEvent(BrowserServer server, String db_path, DatabaseType ty) {
		this.server = server;
		this.db_path = db_path;
		this.ty = ty;
	}
	
	/**
	 * Returns the scion-browser instance creating this event
	 * 
	 * @return
	 */
	public BrowserServer getServer() {
		return this.server;
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
