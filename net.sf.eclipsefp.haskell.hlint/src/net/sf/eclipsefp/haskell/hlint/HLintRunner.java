/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.hlint;

import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.haskell.hlint.parser.OutputParser;
import net.sf.eclipsefp.haskell.hlint.util.HLintText;
import net.sf.eclipsefp.haskell.util.ProcessRunner;

import org.eclipse.core.runtime.IPath;
import org.eclipse.osgi.util.NLS;

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
		StringWriter err=new StringWriter();
		StringWriter out=new StringWriter();
		try {
			String exe=HLintPlugin.getHlintPath();
			if (exe==null || exe.length()==0){
				exe="hlint"; // hope it's in the path
			}
			// code is 1 if we have errors!
			int code=new ProcessRunner().executeBlocking(path.toFile().getParentFile(), out, err,  exe, path.toOSString());
			
			OutputParser parser = new OutputParser(new StringReader(out.toString()));
			List<Suggestion> sugs= parser.suggestions();
			if(err.toString().length()>0){
				HLintPlugin.logError(NLS.bind(HLintText.error_run,err.toString()),null);
			} else if (code!=0 && sugs.size()==0){
				HLintPlugin.logError(NLS.bind(HLintText.error_run,out.toString()),null);
			}
			return sugs;
			
		} catch (Throwable ex) {
			String msg=err.toString().length()>0?err.toString():out.toString();
			HLintPlugin.logError(NLS.bind(HLintText.error_run,msg), ex);
		}
		return new ArrayList<Suggestion>();
	}
	
	public static List<Suggestion> runHLintOn(IPath path) {
		HLintRunner runner = new HLintRunner();
		return runner.run(path);
	}
}
