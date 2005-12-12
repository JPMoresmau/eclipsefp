package net.sf.eclipsefp.haskell.core.jparser;

import de.leiffrenzel.fp.haskell.core.halamo.IMatch;
import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionBinding;
import net.sf.eclipsefp.haskell.core.jparser.ast.Module;

public class ModuleBuilder {

	private Module fModule;
	private FunctionBinding fCurrentFunction = new NullFunctionBinding();

	public void startModule() {
		fModule = new Module();
	}

	public Module getResult() {
		return fModule;
	}

	public void addFunctionMatch(IMatch match) {
		if (!fCurrentFunction.acceptsMatch(match)) {
			fCurrentFunction = createFunctionBinding();
			fCurrentFunction.setName(match.getName());
		}
		fCurrentFunction.addMatch(match);
	}

	private FunctionBinding createFunctionBinding() {
		FunctionBinding function = new FunctionBinding();
		fModule.addDeclaration(function);
		return function;
	}

}
