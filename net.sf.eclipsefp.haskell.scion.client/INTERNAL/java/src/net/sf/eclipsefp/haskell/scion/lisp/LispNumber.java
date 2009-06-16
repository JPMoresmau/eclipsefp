package net.sf.eclipsefp.haskell.scion.lisp;

public class LispNumber extends LispExpr {
	
	double value;
	
	public LispNumber(double value) {
		this.value = value;
	}
	
	public double getDouble() {
		return value;
	}
	
	public int getInt() {
		return (int)value;
	}
	
	@Override
	public String toString() {
		return Double.toString(value);
	}

}
