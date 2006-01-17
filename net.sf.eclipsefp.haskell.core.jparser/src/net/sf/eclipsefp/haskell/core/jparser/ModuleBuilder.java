package net.sf.eclipsefp.haskell.core.jparser;

import de.leiffrenzel.fp.haskell.core.halamo.IConstructor;
import de.leiffrenzel.fp.haskell.core.halamo.IDeclaration;
import de.leiffrenzel.fp.haskell.core.halamo.IExportSpecification;
import de.leiffrenzel.fp.haskell.core.halamo.IImport;
import de.leiffrenzel.fp.haskell.core.halamo.IMatch;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;
import de.leiffrenzel.fp.haskell.core.halamo.ITypeSignature;
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

	public void addFunctionMatch(IMatch match) {
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

	public IModule startModule(String moduleName) {
		fModule = new Module();
		fModule.setName(moduleName);
		
		return fModule;
	}

	public void addDeclaration(IDeclaration declaration) {
		fModule.addDeclaration(declaration);
	}

	public void addExport(IExportSpecification export) {
		fModule.addExport(export);
	}

	public void addImport(IImport theImport) {
		fModule.addImport(theImport);
	}

	public ClassDeclaration startClassDeclaration() {
		fClassDeclaration = new ClassDeclaration();
		addDeclaration(fClassDeclaration);
		return fClassDeclaration;
	}

	public void addTypeSignature(ITypeSignature tsig) {
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

	public void addConstructor(IConstructor cons) {
		if (fDataDeclaration.accepts(cons)) {
			fDataDeclaration.addConstructor(cons);
		}
	}

}
