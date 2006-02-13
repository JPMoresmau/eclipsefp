package de.leiffrenzel.fp.haskell.core.halamo;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;

import de.leiffrenzel.fp.haskell.core.HaskellCorePlugin;
import de.leiffrenzel.fp.haskell.core.parser.IHaskellParser;
import de.leiffrenzel.fp.haskell.core.parser.ParserManager;

public class ScopeCalculator {

	public Scope computeScopeFor(IFile file) {
		Scope result = new Scope();
		
		IHaskellParser parser = ParserManager.getInstance().getParser();
		//TODO delegate to a lazy parser here
		try {
			ICompilationUnit unit = parser.parse(file);
			
			IImport[] imports = unit.getModules()[0].getImports();
			ModuleSearcher searcher = new ModuleSearcher(
					                          imports[0].getImportedElement(),
					                          result);
			file.getProject().accept(searcher);
		} catch (CoreException ex) {
			//in case something strange occurs, just return the calculated
			//scope the way it is
			HaskellCorePlugin.log("Error ocurred while calculating scope for "
					             + file.getName(), ex);
		}
		
		return result;
	}

	private static class ModuleSearcher implements IResourceVisitor {
		private String fModuleName;
		private Scope fScope;

		public ModuleSearcher(String moduleName, Scope collector) {
			fModuleName = moduleName;
			fScope = collector;
		}

		public boolean visit(IResource resource) throws CoreException {
			if ( resource.isAccessible()
			   && hasHaskellSuffix(resource.getName()))
			{
				IFile file = (IFile) resource.getAdapter(IFile.class);
				
				if (file == null)
					return false;
				
				IHaskellParser parser = ParserManager.getInstance().getParser();
				ICompilationUnit unit = parser.parse(file);
				
				IModule mod = unit.getModules()[0];
				
				if (fModuleName.equals(mod.getName())) {
					fScope.addAvailableModule(mod);
				}
			}
			return true;
		}

		private boolean hasHaskellSuffix(String name) {
			return (name.endsWith(".hs") || name.endsWith(".lhs"));
		}
	}
	
}
