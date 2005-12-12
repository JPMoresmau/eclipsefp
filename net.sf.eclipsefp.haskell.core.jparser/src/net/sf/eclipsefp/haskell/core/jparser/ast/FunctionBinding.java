package net.sf.eclipsefp.haskell.core.jparser.ast;

import java.util.List;
import java.util.Vector;

import de.leiffrenzel.fp.haskell.core.halamo.IFunctionBinding;
import de.leiffrenzel.fp.haskell.core.halamo.IMatch;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

public class FunctionBinding extends Declaration implements IFunctionBinding {

	private List<IMatch> fMatches = new Vector<IMatch>();
	
	public IMatch[] getMatches() {
		return fMatches.toArray(new IMatch[fMatches.size()]);
	}

	public IModule getModule() {
		// TODO Auto-generated method stub
		return null;
	}

	public boolean acceptsMatch(IMatch match) {
		return match.getName().equals(this.getName());
	}

	public void addMatch(IMatch match) {
		if (acceptsMatch(match)) {
			fMatches.add(match);
		} else {
			throw new IllegalArgumentException("Invalid match");
		}
	}

}
