package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.common.core.util.StreamMultiplexer;

public class ProcessRunner implements IProcessRunner {

	private IProcessFactory fProcessFactory;

	public ProcessRunner() {
		this(new ProcessFactory());
	}
	
	public ProcessRunner(IProcessFactory factory) {
		fProcessFactory = factory;
	}

	public String execute(File workingDir, Writer out,
			              Writer err, String... args)
	{
		List<Exception> excList = new ArrayList<Exception>();
		StringWriter output = new StringWriter();
		StringWriter errors = new StringWriter();
		try {
			Process proc = fProcessFactory.startProcess(workingDir, args);
			Thread outRedirect = new StreamMultiplexer("output_redirect",
					  							       proc.getInputStream(),
													   output, out);
			Thread errRedirect = new StreamMultiplexer("error_redirect",
                                                       proc.getErrorStream(),
													   errors, err);
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

		return errors.toString() + output.toString();
	}

}
