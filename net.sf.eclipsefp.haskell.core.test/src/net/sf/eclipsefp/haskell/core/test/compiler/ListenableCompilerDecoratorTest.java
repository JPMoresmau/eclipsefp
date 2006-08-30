package net.sf.eclipsefp.haskell.core.test.compiler;

import static net.sf.eclipsefp.haskell.core.test.compiler.CompilerTestUtil.*;

import org.eclipse.core.resources.IFile;

import net.sf.eclipsefp.haskell.core.compiler.ICompilerListener;
import net.sf.eclipsefp.haskell.core.compiler.IHaskellCompiler;
import net.sf.eclipsefp.haskell.core.compiler.ListenableCompilerDecorator;
import junit.framework.TestCase;

public class ListenableCompilerDecoratorTest extends TestCase {
	
	public void testRedirectsCompilerOutputToOneListener() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener listener = createListener();
		
		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(listener);
		
		testedCompiler.compile((IFile) null);
		
		assertReceivedExpectedOutput(listener);
	}

	public void testRedirectsCompilerOutputToMoreThanOneListener() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener fstListener = createListener();
		ICompilerListener sndListener = createListener();
		ICompilerListener trdListener = createListener();
		
		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(fstListener);
		testedCompiler.addListener(sndListener);
		testedCompiler.addListener(trdListener);
		
		testedCompiler.compile((IFile) null);
		
		assertReceivedExpectedOutput(fstListener);
		assertReceivedExpectedOutput(sndListener);
		assertReceivedExpectedOutput(trdListener);
	}
	
	public void testCompileMoreThanOnce() {
		IHaskellCompiler realCompiler = new StubCompiler();
		ICompilerListener listener = createListener();
		
		ListenableCompilerDecorator testedCompiler =
			new ListenableCompilerDecorator(realCompiler);
		testedCompiler.addListener(listener);
		
		testedCompiler.compile((IFile) null);
		assertReceivedExpectedOutput(listener);

		ICompilerListener otherListener = createListener();
		testedCompiler.addListener(otherListener);
	
		testedCompiler.compile((IFile) null);
		assertReceivedExpectedOutput(otherListener);
	}

}
