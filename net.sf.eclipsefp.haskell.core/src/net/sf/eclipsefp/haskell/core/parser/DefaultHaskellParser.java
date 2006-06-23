// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.halamo.*;

/**
 * <p>
 * The default implementation for the Haskell parser. This is a dummy
 * implementation which does actually no parsing at all.
 * </p>
 * 
 * @author Leif Frenzel
 */
class DefaultHaskellParser implements IHaskellParser {

	// interface methods of IHaskellParser
	// ////////////////////////////////////
	public ICompilationUnit parse(final IFile file) throws CoreException {
		return createDummyCompilationUnit();
	}

	public ICompilationUnit parse(IFile file,
		String sourceCode) throws CoreException
	{
		return createDummyCompilationUnit();
	}

	public boolean canParse() {
		// this parser should not be used, so return always false
		return false;
	}

	// helping methods
	// ////////////////

	private ICompilationUnit createDummyCompilationUnit() {
		String msg = "Parsing should not have been invoked on default parser, "
				+ "check canParse() before calling parser.";
		HaskellCorePlugin.log(msg, IStatus.WARNING);

		DummyCompilationUnit result = new DummyCompilationUnit();
		result.setModules(parseModules(result));
		return result;
	}

	private IModule[] parseModules(final ICompilationUnit compilationUnit) {
		String msg = "Parsing should not have been invoked on default parser, "
				+ "check canParse() before calling parser.";
		HaskellCorePlugin.log(msg, IStatus.WARNING);
		return createDummyModules(compilationUnit);
	}

	private IModule[] createDummyModules(final ICompilationUnit cu) {
		return new IModule[] { new IModule() {
			public IExportSpecification[] getExportSpecifications() {
				return new IExportSpecification[0];
			}

			public IImport[] getImports() {
				return new IImport[0];
			}

			public IDeclaration[] getDeclarations() {
				return new IDeclaration[0];
			}

			public ICompilationUnit getCompilationUnit() {
				return cu;
			}

			public String getName() {
				return "[Could not parse anything, no parser found.]";
			}

			public ISourceLocation getSourceLocation() {
				return createDummySourceLocation();
			}

			public IHaskellLanguageElement getParent() {
				// modules are the top-level elements of the language element
				// hierarchy
				// and have no parent
				return null;
			}
		} };
	}

	private ISourceLocation createDummySourceLocation() {
		return new ISourceLocation() {
			public int getColumn() {
				return 0;
			}

			public int getLine() {
				return 0;
			}

			public boolean isAfter(final ISourceLocation anotherLocation) {
				// unused
				return false;
			}

			public boolean isBefore(final ISourceLocation anotherLocation) {
				// unused
				return false;
			}

			public long getOffset() {
				return 0;
			}
		};
	}

	// inner classes
	// //////////////

	private class DummyCompilationUnit implements ICompilationUnit {

		private final IFile file;

		private IModule[] modules;

		DummyCompilationUnit(final IFile file) {
			this.file = file;
		}

		public DummyCompilationUnit() {
			this.file = null;
		}

		void setModules(final IModule[] modules) {
			this.modules = modules;
		}

		// interface methods of ICompilationUnit
		// //////////////////////////////////////

		public IModule[] getModules() {
			return modules;
		}

		public IFile getUnderlyingResource() {
			return file;
		}

		public ISourceLocation getNextLocation(final ISourceLocation srcLoc) {
			return null;
		}

		public Object getAdapter(final Class adapter) {
			Object result = null;
			if (adapter.getClass() == IResource.class) {
				result = file;
			}
			return result;
		}

		public String getOriginalSourceCode() {
			return "";
		}

	}

}
