package net.sf.eclipsefp.haskell.core.parser.test.util;


import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.parser.IHaskellParser;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.core.test.project.util.MockFile;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

public class Parser_PDETestCase extends TestCase {

	public ICompilationUnit parseAsFile(String input) throws CoreException {
		return parse(new MockFile(input));
	}
	
	public ICompilationUnit parseAsString(String input) throws CoreException {
		IHaskellParser parser = ParserManager.getInstance().getParser();
		
		return parser.parse(new MockFile(input), input);
	}

	public ICompilationUnit parse(IFile input) throws CoreException {
		IHaskellParser parser = ParserManager.getInstance().getParser();
		
		return parser.parse(input);
	}

}
