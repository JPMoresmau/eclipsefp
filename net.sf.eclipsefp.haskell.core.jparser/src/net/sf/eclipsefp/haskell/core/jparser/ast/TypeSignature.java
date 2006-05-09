package net.sf.eclipsefp.haskell.core.jparser.ast;

import net.sf.eclipsefp.haskell.core.halamo.ITypeSignature;

public class TypeSignature extends Declaration implements ITypeSignature {

	private String[] fIdentifiers = new String[0];
	
	public String[] getIdentifiers() {
		return fIdentifiers;
	}

	public void setIdentifiers(String[] ids) {
		assert null != ids;
		fIdentifiers = ids.clone();

		setName(assembleName(ids));
	}

	private String assembleName(String[] ids) {
		assert 0 < ids.length;

		StringBuffer buf = new StringBuffer();
		buf.append(ids[0]);
		for(int i = 1; i < ids.length; ++i) {
			buf.append("");
			buf.append(ids[i]);
		}
		String name = buf.toString();
		return name;
	}

}
