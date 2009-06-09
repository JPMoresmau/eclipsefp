package net.sf.eclipsefp.haskell.scion.lisp;

import net.sf.eclipsefp.haskell.scion.client.ScionParseException;

public class LispExpr {
	
	public LispList asList() {
		if (this instanceof LispList)
			return (LispList)this;
		else
			throw new ScionParseException("Expected a list");
	}
	
	public LispIdentifier asIdentifier() {
		if (this instanceof LispIdentifier)
			return (LispIdentifier)this;
		else
			throw new ScionParseException("Expected an identifier");
	}
	
	public LispString asString() {
		if (this instanceof LispString)
			return (LispString)this;
		else
			throw new ScionParseException("Expected a string");
	}
	
	public LispNumber asNumber() {
		if (this instanceof LispNumber)
			return (LispNumber)this;
		else
			throw new ScionParseException("Expected a number");
	}

}
