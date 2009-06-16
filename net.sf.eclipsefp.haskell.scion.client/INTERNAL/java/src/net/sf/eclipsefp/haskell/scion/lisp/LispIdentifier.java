package net.sf.eclipsefp.haskell.scion.lisp;

public class LispIdentifier extends LispExpr {

	private String name;
	
	public LispIdentifier(String name) {
		this.name = name;
	}
	
	@Override
	public String toString() {
		return name;
	}
	
}
