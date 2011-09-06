/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;

/**
 * Base class for all kinds of suggestions that involve
 * modification of the Haskell source code.
 * @author Alejandro Serrano
 *
 */
public abstract class CodeModification {
	private CodeModificationType type;
	
	public CodeModificationType getType() {
		return this.type;
	}
	
	protected void setType(CodeModificationType type) {
		this.type = type;
	}
}
