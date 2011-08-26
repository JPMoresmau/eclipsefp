/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser;

/**
 * Saves information about an event raised by scion-browser.
 * 
 * @author Alejandro Serrano
 */
public class BrowserEvent {
	BrowserServer server;

	public BrowserEvent(BrowserServer server) {
		this.server = server;
	}

	/**
	 * Returns the scion-browser instance creating this event
	 * 
	 * @return The BrowserServer that raised the event.
	 */
	public BrowserServer getServer() {
		return this.server;
	}
}
