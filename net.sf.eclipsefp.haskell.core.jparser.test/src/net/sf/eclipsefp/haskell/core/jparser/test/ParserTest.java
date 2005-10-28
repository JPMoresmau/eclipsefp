package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringReader;

import net.sf.eclipsefp.haskell.core.jparser.HaskellParser;

import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.halamo.IModule;

import junit.framework.TestCase;

public class ParserTest extends TestCase {
	
	public void testModule() {
		ICompilationUnit unit = parse("module ParserTest where");
		assertNotNull(unit);

		IModule[] mods = unit.getModules();
		
		assertNotNull(mods);
		assertEquals(1, mods.length);
		assertEquals("ParserTest", mods[0].getName());
	}
	
	private ICompilationUnit parse(String contents) {
		HaskellParser parser = new HaskellParser();
		
		return parser.parse(new StringReader(contents));
	}
}
