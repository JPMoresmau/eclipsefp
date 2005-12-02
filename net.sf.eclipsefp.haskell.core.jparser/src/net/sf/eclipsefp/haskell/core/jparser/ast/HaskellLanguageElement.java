package net.sf.eclipsefp.haskell.core.jparser.ast;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IHaskellLanguageElement;
import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;

/**
 * Convenience class for all nodes of the AST
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class HaskellLanguageElement implements IHaskellLanguageElement {

	private String fName = "";
	private SourceLocation fLocation = new SourceLocation();

	protected HaskellLanguageElement() {
	}
	
	public String getName() {
		return fName;
	}

	public void setName(String name) {
		fName = name;
	}

	public ICompilationUnit getCompilationUnit() {
		// TODO Auto-generated method stub
		return null;
	}

	public IHaskellLanguageElement getParent() {
		// TODO Auto-generated method stub
		return null;
	}

	public ISourceLocation getSourceLocation() {
		return fLocation;
	}

	public void setLocation(int line, int column) {
		fLocation.setPoint(line, column);
	}

}
