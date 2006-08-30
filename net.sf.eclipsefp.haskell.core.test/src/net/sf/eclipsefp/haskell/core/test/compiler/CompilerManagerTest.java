package net.sf.eclipsefp.haskell.core.test.compiler;

import static net.sf.eclipsefp.haskell.core.test.compiler.CompilerTestUtil.*;

import org.eclipse.core.resources.IFile;

import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.ICompilerListener;
import junit.framework.TestCase;

public class CompilerManagerTest extends TestCase {

	private CompilerManager manager;

	@Override
	protected void setUp() throws Exception {
		manager = new CompilerManager();
		String compilerID = "stub";
		manager.installCompiler(compilerID, new StubCompiler());
		manager.selectCompiler(compilerID);
	}

	public void testNotifiesCompilerListeners() {
		ICompilerListener listener = createListener();
		manager.addCompilerListener(listener);
		
		manager.getCompiler().compile((IFile) null);
		
		assertReceivedExpectedOutput(listener);
	}

}
