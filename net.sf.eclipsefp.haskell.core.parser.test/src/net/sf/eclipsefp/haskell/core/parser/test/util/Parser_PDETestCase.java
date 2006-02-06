package net.sf.eclipsefp.haskell.core.parser.test.util;

import net.sf.eclipsefp.test.util.common.MockFile;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.parser.IHaskellParser;
import de.leiffrenzel.fp.haskell.core.parser.ParserManager;
import junit.framework.TestCase;

public class Parser_PDETestCase extends TestCase {

	public ICompilationUnit parse(String input) throws CoreException {
		return parse(new MockFile(input));
	}

	public ICompilationUnit parse(IFile input) throws CoreException {
		IHaskellParser parser = ParserManager.getInstance().getParser();
		
		return parser.parse(input);
	}

}
