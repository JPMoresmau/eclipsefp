package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;

import net.sf.eclipsefp.haskell.core.compiler.NullWriter;
import net.sf.eclipsefp.haskell.ghccompiler.core.IProcessFactory;
import net.sf.eclipsefp.haskell.ghccompiler.core.ProcessRunner;

import static org.easymock.EasyMock.*;

import junit.framework.TestCase;

public class ProcessRunnerTest extends TestCase {
	
	public void testReturnsStandardOutput() throws IOException {
		final String expectedResult = "standard output contents";
		IProcessFactory factory = createProcessFactory(expectedResult, "");

		ProcessRunner runner = new ProcessRunner(factory);
		String actualResult = runner.execute(new File("unimportant"),
				                             new NullWriter(),
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
                                             new NullWriter(),
				                             "unimportant");
		assertEquals(expectedResult, actualResult);
	}

	public void testRedirectsOutputStreams() throws IOException {
		final String expectedOut = "standard error stream contents\n";
		final String expectedErr = "standard error stream contents\n";
		IProcessFactory factory = createProcessFactory(expectedOut, expectedErr);

		ProcessRunner runner = new ProcessRunner(factory);
		final StringWriter out = new StringWriter();
		final StringWriter err = new StringWriter();
		runner.execute(new File("unimportant"), out, err, "unimportant");

		assertEquals(expectedOut, out.toString());
		assertEquals(expectedErr, err.toString());
	}

	private IProcessFactory createProcessFactory(String stdout, String stderr) throws IOException {
		IProcessFactory factory = createMock(IProcessFactory.class);
		expect(factory.startProcess((File) anyObject(), (String[]) anyObject()))
			.andReturn(new StubProcess(stdout, stderr));
		replay(factory);
		return factory;
	}

}
