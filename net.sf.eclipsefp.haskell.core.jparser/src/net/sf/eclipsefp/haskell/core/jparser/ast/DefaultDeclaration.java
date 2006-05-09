package net.sf.eclipsefp.haskell.core.jparser.ast;

import net.sf.eclipsefp.haskell.core.halamo.IDefaultDeclaration;

public class DefaultDeclaration extends Declaration implements
		IDefaultDeclaration {
	
	public DefaultDeclaration() {
		this.setName("default declaration");
	}

}
