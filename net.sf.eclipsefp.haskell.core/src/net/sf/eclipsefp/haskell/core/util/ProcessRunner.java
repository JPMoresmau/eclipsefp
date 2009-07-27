package net.sf.eclipsefp.haskell.core.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.Writer;
import net.sf.eclipsefp.haskell.core.internal.util.StreamRedirect;

public class ProcessRunner implements IProcessRunner {

  private final IProcessFactory fProcessFactory;

  public ProcessRunner() {
    this( new ProcessFactory() );
  }

  public ProcessRunner( final IProcessFactory factory ) {
    fProcessFactory = factory;
  }

  public void executeBlocking( final File workingDir, final Writer out,
      final Writer err, final String ... args ) throws IOException {

    Process proc = doExecute( workingDir, args );

    Thread outRedirect = redirect( new InputStreamReader( proc.getInputStream() ), out );
    Thread errRedirect = redirect( new InputStreamReader( proc.getErrorStream() ), err );

    try {
      proc.waitFor(); // wait for process to finish
      outRedirect.join(); // wait until out stream content is redirected
      errRedirect.join(); // wait until err stream content is redirected
    } catch (InterruptedException ex) {
      // ignore
    }
  }

  public Process executeNonblocking( final File workingDir, final Writer out,
      final Writer err, final String ... args ) throws IOException {
    Process proc = doExecute( workingDir, args );
    redirect( new InputStreamReader( proc.getInputStream() ), out );
    redirect( new InputStreamReader( proc.getErrorStream() ), err );
    return proc;
  }

  private Process doExecute( final File workingDir, final String ... args )
      throws IOException {
    Process proc = fProcessFactory.startProcess( workingDir, args );
    return proc;
  }

  private Thread redirect( final Reader in, final Writer out ) {
    Thread outRedirect = new StreamRedirect( "output_redirect", //$NON-NLS-1$
        in, out );
    outRedirect.start();
    return outRedirect;
  }

}
