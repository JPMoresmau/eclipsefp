package net.sf.eclipsefp.haskell.core.jparser;

import net.sf.eclipsefp.haskell.core.halamo.IConstructor;
import net.sf.eclipsefp.haskell.core.halamo.IDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.IExportSpecification;
import net.sf.eclipsefp.haskell.core.halamo.IImport;
import net.sf.eclipsefp.haskell.core.halamo.IMatch;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.ITypeSignature;
import net.sf.eclipsefp.haskell.core.jparser.ast.ClassDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.DataDeclaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.Declaration;
import net.sf.eclipsefp.haskell.core.jparser.ast.FunctionBinding;
import net.sf.eclipsefp.haskell.core.jparser.ast.Module;

public class ModuleBuilder {

	private Module fModule;
	private FunctionBinding fCurrentFunction = new NullFunctionBinding();
	private ClassDeclaration fClassDeclaration = new NullClassDeclaration();
	private DataDeclaration fDataDeclaration = new NullDataDeclaration();

	public IModule startModule() {
		return startModule("");
	}

	public Module getResult() {
		return fModule;
	}

	public void addFunctionMatch(final IMatch match) {
		if (!fCurrentFunction.acceptsMatch(match)) {
			fCurrentFunction = createFunctionBinding();
			fCurrentFunction.setName(match.getName());
			fCurrentFunction.setLocation(match.getSourceLocation());
		}
		fCurrentFunction.addMatch(match);
	}

	private FunctionBinding createFunctionBinding() {
		FunctionBinding function = new FunctionBinding();
		fModule.addDeclaration(function);
		return function;
	}

	public IModule startModule(final String moduleName) {
		fModule = new Module();
		fModule.setName(moduleName);
		
		return fModule;
	}

	public void addDeclaration(final IDeclaration declaration) {
		fModule.addDeclaration(declaration);
	}

	public void addExport(final IExportSpecification export) {
		fModule.addExport(export);
	}

	public void addImport(final IImport theImport) {
		fModule.addImport(theImport);
	}

	public ClassDeclaration startClassDeclaration() {
		fClassDeclaration = new ClassDeclaration();
		addDeclaration(fClassDeclaration);
		return fClassDeclaration;
	}

	public void addTypeSignature(final ITypeSignature tsig) {
		if (fClassDeclaration.accepts(tsig)) {
			fClassDeclaration.addTypeSignature(tsig);
		} else {
			fModule.addDeclaration(tsig);
		}
	}

	public void endClassDeclaration() {
		fClassDeclaration = new NullClassDeclaration();
	}

	public Declaration startDataDeclaration() {
		fDataDeclaration = new DataDeclaration();
		addDeclaration(fDataDeclaration);
		return fDataDeclaration;
	}

	public void addConstructor(final IConstructor cons) {
		if (fDataDeclaration.accepts(cons)) {
			fDataDeclaration.addConstructor(cons);
		}
	}

}
