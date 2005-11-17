package net.sf.eclipsefp.haskell.core.jparser.test;

import java.io.StringBufferInputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

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
public class PluginTest extends TestCase {

	public void testVisibleToCore() {
		IHaskellParser parser = ParserManager.getInstance().getParser();
		
		assertEquals(JavaParserBridge.class, parser.getClass());
	}
	
	public void testParseFileResource() throws CoreException {
	    IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
	    
	    IProject project = root.getProject("foo");
	    project.create(null);
	    project.open(null);
	    
	    IFile file = project.getFile("bar.hs");
	    
	    file.create(new StringBufferInputStream("module Empty where {}"), true, null);
	    
		IHaskellParser parser = ParserManager.getInstance().getParser();
		
		ICompilationUnit unit = parser.parse(file);
		
		assertNotNull(unit);
		assertEquals(1, unit.getModules().length);
		assertEquals("Empty", unit.getModules()[0].getName());
	}
	
}
