/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;

import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.hlint.parser.OutputParser;
import net.sf.eclipsefp.haskell.hlint.util.HLintText;
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
			StringWriter out=new StringWriter();
			Writer err=new StringWriter();
			String exe=HLintPlugin.getHlintPath();
			if (exe==null || exe.length()==0){
				exe="hlint"; // hope it's in the path
			}
			new ProcessRunner().executeBlocking(path.toFile().getParentFile(), out, err,  exe, path.toOSString());
			
			OutputParser parser = new OutputParser(new StringReader(out.toString()));
			return parser.suggestions();
		} catch (Throwable ex) {
			HLintPlugin.logError(HLintText.error_run, ex);
		}
		return new ArrayList<Suggestion>();
	}
	
	public static List<Suggestion> runHLintOn(IPath path) {
		HLintRunner runner = new HLintRunner();
		return runner.run(path);
	}
}
