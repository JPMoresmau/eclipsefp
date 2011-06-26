package net.sf.eclipsefp.haskell.hlint;

public class Suggestion {
	private SourceLocation location;
	private Severity severity;
	private String message;
	private CodeModification prev;
	private CodeModification post;
	
	public SourceLocation getLocation() {
		return location;
	}
	
	public Severity getSeverity() {
		return severity;
	}
	
	public String getMessage() {
		return message;
	}
	
	public CodeModification getPrev() {
		return prev;
	}
	
	public CodeModification getPost() {
		return post;
	}

	void setLocation(SourceLocation location) {
		this.location = location;
	}

	void setSeverity(Severity severity) {
		this.severity = severity;
	}

	void setMessage(String message) {
		this.message = message;
	}

	void setPrev(CodeModification prev) {
		this.prev = prev;
	}

	void setPost(CodeModification post) {
		this.post = post;
	}
}
