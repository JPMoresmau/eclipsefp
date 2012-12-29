/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;

import org.eclipse.core.resources.IFile;
import org.json.JSONArray;
import org.json.JSONException;

/**
 * A reference to something: the location and the name of the thing referenced
 * @author JP Moresmau
 *
 */
public class ReferenceLocation extends Location {
	/**
	 * the name of the thing
	 */
	private String reference;
	/** 
	 * are we a module?
	 */
	private boolean module;
	
	/**
	 * @param f
	 * @param json
	 * @throws JSONException
	 */
	public ReferenceLocation(String reference,IFile f, JSONArray json) throws JSONException {
		super(f, json);
		setReference(reference);
	}

	public String getReference() {
		return reference;
	}

	public void setReference(String reference) {
		this.reference = reference;
	}

	/* (non-Javadoc)
	 * @see net.sf.eclipsefp.haskell.buildwrapper.types.Location#toString()
	 */
	@Override
	public String toString() {
		return reference+":"+super.toString();
	}

	public boolean isModule() {
		return module;
	}

	public void setModule(boolean module) {
		this.module = module;
	}
}
