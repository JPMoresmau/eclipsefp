package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import static net.sf.eclipsefp.haskell.ghccompiler.test.util.AssertCompilerOutput.*;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerOutput;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcOutputParser;
import junit.framework.TestCase;

public class GhcOutputParserTest extends TestCase {
	
	public void testOneSingleLineError() {
		ICompilerOutput output = GhcOutputParser.parse(
		    "\nMain.hs:3:25-27: Not in scope: `fac'\n");
		
		assertContains(3, 25, 27, "Not in scope: `fac'", output.getErrors());
	}

}
