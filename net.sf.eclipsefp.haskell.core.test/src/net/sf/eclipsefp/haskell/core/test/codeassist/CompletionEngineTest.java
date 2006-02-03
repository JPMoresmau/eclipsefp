package net.sf.eclipsefp.haskell.core.test.codeassist;

import junit.framework.TestCase;

public class CompletionEngineTest extends TestCase {
	
	public void testEasyCompletion() {
		final String input = "module CompletionEngineTest where\n" +
				             "\n" +
				             "putStr str = str\n" +
				             "\n" +
				             "main = pu\n";
		final ICompilationUnit unit = parse(input);
		final CompletionEngine engine = new CompletionEngine(unit);
		
		assertEquals('u', input.charAt(63 - 1));
		
		String[] proposals = engine.complete(unit, 63);
		
		assertEquals("putStr", proposals[0]);
	}

}
