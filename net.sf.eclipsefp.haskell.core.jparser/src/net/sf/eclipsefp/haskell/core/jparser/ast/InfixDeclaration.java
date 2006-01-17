package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.halamo.IInfixDeclaration;

public class InfixDeclaration extends Declaration implements IInfixDeclaration {

	private int fAssociativity;
	private int fPrecedence = 9;
	private List<String> fOperators = new Vector<String>();

	public int getAssociativity() {
		return fAssociativity;
	}

	public int getPrecedenceLevel() {
		return fPrecedence;
	}

	public String[] getOperators() {
		return fOperators .toArray(new String[fOperators.size()]);
	}

	public void setAssociativity(int associativity) {
		fAssociativity = associativity;
	}

	public void setPrecedence(int precedence) {
		fPrecedence = precedence;
	}

	public void addOperators(List<String> operators) {
		fOperators.addAll(operators);
	}

}
