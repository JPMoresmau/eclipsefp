/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;


/**
 * Represents a suggestion of removing some code.
 * @author Alejandro Serrano
 *
 */
public class CodeModificationRemove extends CodeModification {
	
	public CodeModificationRemove() {
		setType(CodeModificationType.REMOVE);
	}
	
	/* (non-Javadoc)
	 * @see net.sf.eclipsefp.haskell.hlint.CodeModification#toJSON()
	 */
	@Override
	public Object toJSON() {
		return CodeModificationType.REMOVE.toString();
	}
}
