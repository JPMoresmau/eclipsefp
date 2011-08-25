/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;

import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.hlint.parser.OutputParser;
import net.sf.eclipsefp.haskell.util.ProcessRunner;

import org.eclipse.core.runtime.IPath;

/**
 * Class the encapsulates the logic of calling hlint and
 * sending the output to the parser to generate a list
 * of suggestions.
 * 
 * @author Alejandro Serrano
 * @author JP Moresmau
 * 
 */
public class HLintRunner {
	
	public List<Suggestion> run(IPath path) {
		try {
			// Run the command
			//String[] cmdLine = new String[] { "hlint", path.toOSString() };
			//Process p = Runtime.getRuntime().exec(cmdLine);
			StringWriter out=new StringWriter();
			Writer err=new StringWriter();
			new ProcessRunner().executeBlocking(path.toFile().getParentFile(), out, err,  "hlint", path.toOSString());
			
			OutputParser parser = new OutputParser(new StringReader(out.toString()));
			// Parse the output
			//p.waitFor();
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
