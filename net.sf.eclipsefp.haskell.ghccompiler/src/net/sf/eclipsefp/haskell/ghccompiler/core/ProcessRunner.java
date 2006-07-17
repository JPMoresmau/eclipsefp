package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.common.core.util.StreamRedirect;

class ProcessRunner implements IProcessRunner {

	public String execute(File workingDir, String... args) {
		List<Exception> excList = new ArrayList<Exception>();
		StringWriter output = new StringWriter();
		StringWriter errors = new StringWriter();
		try {
			Process proc = Runtime.getRuntime().exec(args, null, workingDir);
			StreamRedirect outRedirect = new StreamRedirect("output_redirect",
															proc.getInputStream(),
															output);
			StreamRedirect errRedirect = new StreamRedirect("error_redirect",
															proc.getErrorStream(),
															errors);
			outRedirect.start();
			errRedirect.start();
			proc.waitFor(); // wait for compiler to finish
			outRedirect.join(); // wait until out stream content is redirected
			errRedirect.join(); // wait until err stream content is redirected
		} catch (Exception e) {
			excList.add(e);
		} finally {
			output.flush();
			errors.flush();
		}

		return errors.toString() + "\n" + output.toString();
	}

}
