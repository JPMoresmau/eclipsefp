package net.sf.eclipsefp.haskell.hlint;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.hlint.parser.OutputParser;

import org.eclipse.core.runtime.IPath;

public class HLintRunner {
	
	public List<Suggestion> run(IPath path) {
		try {
			// Run the command
			String[] cmdLine = new String[] { "hlint", path.toOSString() };
			Process p = Runtime.getRuntime().exec(cmdLine);
			// Parse the output
			p.waitFor();
			OutputParser parser = new OutputParser(p.getInputStream());
			return parser.suggestions();
		} catch (Throwable ex) {
			return new ArrayList<Suggestion>();
		}
	}
	
	public static List<Suggestion> runHLintOn(IPath path) {
		HLintRunner runner = new HLintRunner();
		return runner.run(path);
	}
}
