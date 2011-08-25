/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser;

/**
 * Listener interface for loading and unloading databases.
 * 
 * @author Alejandro Serrano
 */
public interface IDatabaseLoadedListener {

	void databaseLoaded(DatabaseLoadedEvent e);
	
	void databaseUnloaded(BrowserEvent e);
}
