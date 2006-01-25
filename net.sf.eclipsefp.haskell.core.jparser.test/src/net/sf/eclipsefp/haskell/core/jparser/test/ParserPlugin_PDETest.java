package net.sf.eclipsefp.haskell.core.jparser.test;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

import antlr.TokenStreamException;

import net.sf.eclipsefp.haskell.core.jparser.JParserPlugin;
import net.sf.eclipsefp.haskell.core.jparser.JavaParserBridge;
import de.leiffrenzel.fp.haskell.core.halamo.ICompilationUnit;
import de.leiffrenzel.fp.haskell.core.parser.IHaskellParser;
import de.leiffrenzel.fp.haskell.core.parser.ParserManager;
import junit.framework.TestCase;

/**
 * Sanity checks for the JParser plugin.
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class ParserPlugin_PDETest extends TestCase {

	public void testConstructorCalled() {
		assertNotNull(JParserPlugin.getDefault());
	}
	
	public void testVisibleToCore() {
		IHaskellParser parser = ParserManager.getInstance().getParser();
		
		assertEquals(JavaParserBridge.class, parser.getClass());
	}
	
	public void testParseFileResource() throws CoreException {
	    IFile file = new MockFile("module Empty where {}");
	    
		IHaskellParser parser = ParserManager.getInstance().getParser();
		
		ICompilationUnit unit = parser.parse(file);
		
		assertNotNull(unit);
		assertEquals(1, unit.getModules().length);
		assertEquals("Empty", unit.getModules()[0].getName());
	}
	
	public void testClosesStream() throws CoreException {
	    MockFile file = new MockFile("module Empty where {}");
	    
		IHaskellParser parser = ParserManager.getInstance().getParser();
		
		parser.parse(file);
		
		file.verify();
	}
	
	public void testClosesStreamOnLiterateFile() throws CoreException {
	    MockFile file = new MockFile("Mock.lhs", "> module Empty where {}");

	    IHaskellParser parser = ParserManager.getInstance().getParser();
		
		parser.parse(file);
		
		file.verify();
	}
	
	public void testClosesStreamOnScanningError() {
	    MockFile file = new MockFile("module Empty where { fat 0 = 0o8 }");
	    
		IHaskellParser parser = ParserManager.getInstance().getParser();
		
		try {
			parser.parse(file);
			fail("Should have raised a scanning error");
		} catch (CoreException e) {
			Throwable cause = e.getStatus().getException();
			assertTrue(cause instanceof TokenStreamException);
		}
		
		file.verify();
	}
}
