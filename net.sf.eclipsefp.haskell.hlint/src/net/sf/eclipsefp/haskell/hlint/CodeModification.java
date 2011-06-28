package net.sf.eclipsefp.haskell.hlint;

public abstract class CodeModification {
	private CodeModificationType type;
	
	public CodeModificationType getType() {
		return this.type;
	}
	
	protected void setType(CodeModificationType type) {
		this.type = type;
	}
}
