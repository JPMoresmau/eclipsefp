/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser;

/**
 * Listener interface for the availability of Hoogle search.
 * 
 * @author Alejandro Serrano
 */
public interface IHoogleLoadedListener {

	void hoogleLoaded(BrowserEvent e);
	
	void hoogleUnloaded(BrowserEvent e);
}
