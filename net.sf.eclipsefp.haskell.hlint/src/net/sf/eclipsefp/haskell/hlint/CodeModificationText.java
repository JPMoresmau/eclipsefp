package net.sf.eclipsefp.haskell.hlint;

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
