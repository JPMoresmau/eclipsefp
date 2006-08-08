package net.sf.eclipsefp.haskell.ghctest.lib;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;

public class CommandRunner {

	private static final int BUFFER_SIZE = 255;
	private RunnerRuntime fRuntime;

	public CommandRunner(RunnerRuntime runtime) {
		fRuntime = runtime;
	}

	public String run(String command) {
		try {
			Process process = fRuntime.exec(command);
			Reader input = new InputStreamReader(process.getInputStream());
			char[] inBuf = new char[BUFFER_SIZE];
			StringBuffer outBuf = new StringBuffer(BUFFER_SIZE);
			int n = input.read(inBuf);
			while(-1 != n) {
				outBuf.append(inBuf, 0, n);
				n = input.read(inBuf);
			}
			return outBuf.toString();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return null;
	}

}
