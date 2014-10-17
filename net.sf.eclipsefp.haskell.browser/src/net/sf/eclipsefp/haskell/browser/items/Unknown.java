/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.browser.items;

/**
 * A unknown Documented, for when we have failed to get more info and we just have a name 
 * @author JP Moresmau
 *
 */
public class Unknown extends Documented {
	private String name;
	
	/**
	 * 
	 */
	public Unknown(String name) {
		this.name=name;
	}

	/* (non-Javadoc)
	 * @see net.sf.eclipsefp.haskell.browser.items.Documented#getName()
	 */
	@Override
	public String getName() {
		return name;
	}

	/* (non-Javadoc)
	 * @see net.sf.eclipsefp.haskell.browser.items.Documented#getCompleteDefinition()
	 */
	@Override
	public String getCompleteDefinition() {
		return name;
	}

	/* (non-Javadoc)
	 * @see net.sf.eclipsefp.haskell.browser.items.Documented#isType()
	 */
	@Override
	public boolean isType() {
		return false;
	}

}
