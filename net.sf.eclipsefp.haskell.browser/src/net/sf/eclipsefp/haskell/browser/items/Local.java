/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.browser.items;

/**
 * Local binding
 * @author JP Moresmau
 *
 */
public class Local extends Documented {
	private String name;
	
	private String signature;

	public Local(String doc, String name, String signature) {
		this.setDoc(doc);
		this.setName(name);
		this.signature = signature;
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
		StringBuilder builder = new StringBuilder(this.getName());
		builder.append(" :: ");
		builder.append(this.getSignature());
		return builder.toString();
	}

	public String getSignature() {
		return signature;
	}

	public void setSignature(String signature) {
		this.signature = signature;
	}

	public void setName(String name) {
		this.name = name;
	}

	/* (non-Javadoc)
	 * @see net.sf.eclipsefp.haskell.browser.items.Documented#isType()
	 */
	@Override
	public boolean isType() {
		return false;
	}
}
