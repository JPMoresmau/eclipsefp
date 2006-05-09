package net.sf.eclipsefp.haskell.core.halamo;

import java.util.Arrays;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.CoreException;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.parser.IHaskellParser;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;

public class ScopeCalculator {

	public Scope computeScopeFor(IFile file) {
		Scope result = new Scope();
		
		IHaskellParser parser = ParserManager.getInstance().getParser();
		//TODO delegate to a lazy parser here
		try {
			ICompilationUnit unit = parser.parse(file);
			
			IImport[] imports = unit.getModules()[0].getImports();
			String[] importedModules = new String[imports.length];
			for(int i = 0; i < imports.length; ++i) {
				importedModules[i] = imports[i].getImportedElement();
			}
			ModuleSearcher searcher = new ModuleSearcher(
					                          result,
					                          importedModules);
			IProject project = file.getProject();
			if (project != null) {
				project.accept(searcher);
			}
		} catch (CoreException ex) {
			//in case something strange occurs, just return the calculated
			//scope the way it is
			HaskellCorePlugin.log("Error ocurred while calculating scope for "
					             + file.getName(), ex);
		}
		
		return result;
	}

	private static class ModuleSearcher implements IResourceVisitor {
		private String[] fModuleNames;
		private Scope fScope;

		public ModuleSearcher(Scope collector, final String... moduleNames) {
			Arrays.sort(moduleNames);
			fModuleNames = moduleNames;
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
				
				if (Arrays.binarySearch(fModuleNames, mod.getName()) >= 0) {
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
