package net.sf.eclipsefp.haskell.core.jparser.ast;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IFunctionBinding;
import de.leiffrenzel.fp.haskell.core.halamo.IHaskellLanguageElement;
import de.leiffrenzel.fp.haskell.core.halamo.IMatch;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.ISourceLocation;

public class FunctionBinding extends Declaration implements IFunctionBinding {

	private String fName;

	public IMatch[] getMatches() {
		return new IMatch[0];
	}

	public IModule getModule() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getName() {
		return fName;
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
		// TODO Auto-generated method stub
		return null;
	}

	public void setName(String name) {
		fName = name;
	}

}
