package net.sf.eclipsefp.haskell.ghctest.lib;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

public class CommandRunner {

	private static final int BUFFER_SIZE = 255;
	private RunnerRuntime fRuntime;

	public CommandRunner() {
		this(new RunnerRuntime());
	}

	public CommandRunner(RunnerRuntime runtime) {
		fRuntime = runtime;
	}

	public String run(String command) {
		try {
			Process process = fRuntime.exec(command);
			Reader std = new InputStreamReader(process.getInputStream());
			Reader err = new InputStreamReader(process.getErrorStream());
			StringBuffer buf = new StringBuffer(BUFFER_SIZE);
			consume(err, buf);
			consume(std, buf);
			return buf.toString();
		} catch (IOException e) {
			return "";
		}
	}

	public static void consume(Reader input, StringBuffer outBuf) throws IOException {
		char[] inBuf = new char[BUFFER_SIZE];
		int n;
		while(-1 != (n = input.read(inBuf))) {
			outBuf.append(inBuf, 0, n);
		}
	}

}
