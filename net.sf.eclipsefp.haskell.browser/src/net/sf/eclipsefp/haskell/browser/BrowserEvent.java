/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.browser;

public class BrowserEvent {
	BrowserServer server;
	
	public BrowserEvent(BrowserServer server) {
		this.server = server;
	}
	
	/**
	 * Returns the scion-browser instance creating this event
	 * 
	 * @return
	 */
	public BrowserServer getServer() {
		return this.server;
	}
}
