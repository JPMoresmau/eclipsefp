package net.sf.eclipsefp.haskell.core.test.codeassist;

import org.eclipse.core.runtime.CoreException;

import net.sf.eclipsefp.haskell.core.codeassist.CompletionEngine;
import net.sf.eclipsefp.haskell.core.parser.test.util.Parser_PDETestCase;
import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;

public class CompletionEngine_PDETest extends Parser_PDETestCase {
	
	public void testDeclarationCompletion() throws CoreException {
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
	
	public void testPreludeClassCompletion() throws CoreException {
		final String input = "module CompletionEngineTest where\n" +
                             "\n" +
                             "fat :: N";
		final ICompilationUnit unit = parse(input);
		final CompletionEngine engine = new CompletionEngine();

		assertEquals('N', input.charAt(43 - 1));

		String[] proposals = engine.complete(unit, 43);

		assertEquals("Num", proposals[0]);
	}
	
	public void testKeywordCompletion() throws CoreException {
		final String input = "module CompletionEngineTest wh";
		final ICompilationUnit unit = parse(input);
		final CompletionEngine engine = new CompletionEngine();

		assertEquals('h', input.charAt(30 - 1));

		String[] proposals = engine.complete(unit, 30);

		assertEquals("where", proposals[0]);
	}
	
	//TODO test if the proposals really start with the preffix
	
	//TODO use preffix after non-whitespace char, like a left paren '(' for example

}
