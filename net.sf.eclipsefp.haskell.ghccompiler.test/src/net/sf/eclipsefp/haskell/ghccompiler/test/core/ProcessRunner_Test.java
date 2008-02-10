package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.compiler.NullWriter;
import net.sf.eclipsefp.haskell.ghccompiler.core.IProcessFactory;
import net.sf.eclipsefp.haskell.ghccompiler.core.ProcessRunner;

public class ProcessRunner_Test extends TestCase {

	public void testReturnsStandardOutput() throws IOException {
		final String expectedResult = "standard output contents";
		IProcessFactory factory = createProcessFactory(expectedResult, "");

		ProcessRunner runner = new ProcessRunner(factory);
		String actualResult = runner.execute(new File("unimportant"),
				                             new NullWriter(),
				                             "unimportant");
		assertEquals(expectedResult, actualResult);
	}

	public void testReturnsStandardError() throws IOException {
		final String expectedResult = "standard error stream contents\n";
		IProcessFactory factory = createProcessFactory("", expectedResult);

		ProcessRunner runner = new ProcessRunner(factory);
		String actualResult = runner.execute(new File("unimportant"),
                                             new NullWriter(),
				                             "unimportant");
		assertEquals(expectedResult, actualResult);
	}

	public void testMergesOutputStreams() throws IOException {
		final String expectedOut = "standard output stream contents\n";
		final String expectedErr = "standard error stream contents\n";
		IProcessFactory factory = createProcessFactory(expectedOut, expectedErr);

		ProcessRunner runner = new ProcessRunner(factory);
		final StringWriter out = new StringWriter();
		runner.execute(new File("unimportant"), out, "unimportant");

		assertTrue(out.toString().contains(expectedOut));
		assertTrue(out.toString().contains(expectedErr));
	}

	private IProcessFactory createProcessFactory(final String stdout, final String stderr) throws IOException {
		IProcessFactory factory = createMock(IProcessFactory.class);
		expect(factory.startProcess((File) anyObject(), (String[]) anyObject()))
			.andReturn(new StubProcess(stdout, stderr));
		replay(factory);
		return factory;
	}

}
