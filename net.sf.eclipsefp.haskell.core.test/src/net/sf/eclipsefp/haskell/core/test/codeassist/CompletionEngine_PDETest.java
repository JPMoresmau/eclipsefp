package net.sf.eclipsefp.haskell.core.test.codeassist;

import org.eclipse.core.runtime.CoreException;

import net.sf.eclipsefp.haskell.core.codeassist.CompletionEngine;
import net.sf.eclipsefp.haskell.core.parser.test.util.Parser_PDETestCase;
import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;

public class CompletionEngine_PDETest extends Parser_PDETestCase {
	
	public void testEasyCompletion() throws CoreException {
		final String input = "module CompletionEngineTest where\n" +
				             "\n" +
				             "putStr str = str\n" +
				             "\n" +
				             "main = pu\n";
		final ICompilationUnit unit = parse(input);
		final CompletionEngine engine = new CompletionEngine();
		
		assertEquals('u', input.charAt(62 - 1));
		
		String[] proposals = engine.complete(unit, 62);
		
		assertEquals("putStr", proposals[0]);
	}

}
