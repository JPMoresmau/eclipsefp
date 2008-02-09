package net.sf.eclipsefp.haskell.core.jparser.test;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.jparser.JParserPlugin;
import net.sf.eclipsefp.haskell.core.jparser.JavaParserBridge;
import net.sf.eclipsefp.haskell.core.parser.IHaskellParser;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.core.parser.test.util.Parser_PDETestCase;
import net.sf.eclipsefp.haskell.core.test.project.util.MockFile;

import org.eclipse.core.runtime.CoreException;

import antlr.TokenStreamException;

/**
 * Sanity checks for the JParser plugin.
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class ParserPlugin_PDETest extends Parser_PDETestCase {

	public void testConstructorCalled() {
		assertNotNull(JParserPlugin.getDefault());
	}
	
	public void testVisibleToCore() {
		IHaskellParser parser = ParserManager.getInstance().getParser();
		
		assertEquals(JavaParserBridge.class, parser.getClass());
	}
	
	public void testParseFileResource() throws CoreException {
		ICompilationUnit unit = parseAsFile("module Empty where {}");
		
		assertNotNull(unit);
		assertEquals(1, unit.getModules().length);
		assertEquals("Empty", unit.getModules()[0].getName());
	}
	
	public void testClosesStream() throws CoreException {
	    MockFile file = new MockFile("module Empty where {}");
	    
		parse(file);
		
		file.verify();
	}
	
	public void testClosesStreamOnLiterateFile() throws CoreException {
	    MockFile file = new MockFile("Mock.lhs", "> module Empty where {}");

		parse(file);
		
		file.verify();
	}
	
	public void testClosesStreamOnScanningError() {
	    MockFile file = new MockFile("module Empty where { fat 0 = 0o8 }");
	    
		try {
			parse(file);
			fail("Should have raised a scanning error");
		} catch (CoreException e) {
			Throwable cause = e.getStatus().getException();
			assertTrue(cause instanceof TokenStreamException);
		}
		
		file.verify();
	}
	
	public void testRecordsOriginalSourceCodeFromString() throws CoreException {
		final String source = "module Factorial where\n" +
							  "fat 0 = 1\n" +
							  "fat n = n * (fat (n - 1))";
		
		ICompilationUnit unit = parseAsString(source);
		
		assertEquals(source, unit.getOriginalSourceCode());
	}
	
	public void testRecordsOriginalSourceCodeFromFile() throws CoreException {
		final String source = "module Factorial where\n" +
							  "fat 0 = 1\n" +
							  "fat n = n * (fat (n - 1))";
		
		ICompilationUnit unit = parseAsFile(source);
		
		assertEquals(source, unit.getOriginalSourceCode());
	}
	
	public void testEmptyUnit() throws CoreException {
		final String source = "";
		
		ICompilationUnit unit = parseAsString(source);

		assertEquals(0, unit.getModules().length);
	}
	
	
}
