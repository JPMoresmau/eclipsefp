package net.sf.eclipsefp.haskell.scion.lisp;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.ScionParseException;

public class LispList extends LispExpr {

	private ArrayList<LispExpr> exprs;
	
	/**
	 * Creates the empty list (equivalent to nil).
	 */
	public LispList() {
		this.exprs = new ArrayList<LispExpr>();
	}
	
	public LispList(List<LispExpr> exprs) {
		this.exprs = new ArrayList<LispExpr>(exprs);
	}
	
	public LispExpr get(int i) {
		if (i < exprs.size())
			return exprs.get(i);
		else
			throw new ScionParseException("List is not long enough");
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
