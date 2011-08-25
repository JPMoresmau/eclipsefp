/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
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
}
