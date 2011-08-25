/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;

/**
 * Represents a suggestion of exchanging some text
 * with another text.
 * @author Alejandro Serrano
 *
 */
public class CodeModificationText extends CodeModification {
	private String text;
	
	public CodeModificationText(String text) {
		setType(CodeModificationType.TEXT);
		this.text = text;
	}
	
	public String getText() {
		return this.text;
	}
}
