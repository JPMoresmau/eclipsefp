package net.sf.eclipsefp.haskell.scion.lisp;

public class LispString extends LispExpr {

	private String value;
	
	public LispString(String value) {
		this.value = value;
	}
	
	public String getValue() {
		return value;
	}
	
	/**
	 * Note: this does not escape anything, and will thus not produce valid Lisp code!
	 */
	@Override
	public String toString() {
		StringBuffer buffer = new StringBuffer();
		buffer.append('"');
		buffer.append(value);
		buffer.append('"');
		return buffer.toString();
	}
	
}
