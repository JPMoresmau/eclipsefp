package net.sf.eclipsefp.haskell.scion.lisp;

import java.util.ArrayList;
import java.util.List;

public class LispList extends LispExpr {

	private ArrayList<LispExpr> exprs;
	
	public LispList(List<LispExpr> exprs) {
		this.exprs = new ArrayList<LispExpr>(exprs);
	}
	
	public LispExpr get(int i) {
		return exprs.get(i);
	}
	
	public int length() {
		return exprs.size();
	}
	
	public String toString() {
		StringBuffer buffer = new StringBuffer();
		buffer.append("(");
		boolean space = false;
		for (LispExpr expr : exprs) {
			if (space)
				buffer.append(" ");
			space = true;
			buffer.append(expr.toString());
		}
		buffer.append(")");
		return buffer.toString();
	}
	
}
