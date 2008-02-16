// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.halamo.IDeclaration;
import net.sf.eclipsefp.haskell.core.halamo.IExportSpecification;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;
import net.sf.eclipsefp.haskell.core.halamo.IImport;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.ISourceLocation;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IStatus;

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
	public ICompilationUnit parse(final IFile file) {
		return createDummyCompilationUnit();
	}

	public ICompilationUnit parse(final IFile file,
		final String sourceCode)
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
    String msg = "Parsing should not have been invoked on default parser, " //$NON-NLS-1$
        + "check canParse() before calling parser."; //$NON-NLS-1$
    HaskellCorePlugin.log( msg, IStatus.WARNING );

    DummyCompilationUnit result = new DummyCompilationUnit();
    result.setModules( parseModules( result ) );
    return result;
  }

  private IModule[] parseModules( final ICompilationUnit compilationUnit ) {
    String msg = "Parsing should not have been invoked on default parser, " //$NON-NLS-1$
        + "check canParse() before calling parser."; //$NON-NLS-1$
    HaskellCorePlugin.log( msg, IStatus.WARNING );
    return createDummyModules( compilationUnit );
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
				return "[Could not parse anything, no parser found.]"; //$NON-NLS-1$
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

			public boolean isEmpty() {
				return true;
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
			return ""; //$NON-NLS-1$
		}

	}

}
