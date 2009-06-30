package net.sf.eclipsefp.haskell.core.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.internal.util.MultiplexedWriter;
import net.sf.eclipsefp.haskell.core.internal.util.StreamRedirect;

public class ProcessRunner implements IProcessRunner {

  private final IProcessFactory fProcessFactory;

  public ProcessRunner() {
    this( new ProcessFactory() );
  }

  public ProcessRunner( final IProcessFactory factory ) {
    fProcessFactory = factory;
  }

  public String executeBlocking( final File workingDir, final Writer out,
      final String ... args ) {
    List<Exception> excList = new ArrayList<Exception>();
    StringWriter returnedOut = new StringWriter();
    try {
      Writer outs = new MultiplexedWriter(returnedOut, out);
      Process proc = doExecute( workingDir, args );
      Thread outRedirect = redirect( new InputStreamReader(proc.getInputStream()), outs );

      proc.waitFor(); // wait for process to finish
      outRedirect.join(); // wait until out stream content is redirected
    } catch( Exception e ) {
      excList.add( e );
    } finally {
      returnedOut.flush();
    }

    return returnedOut.toString();
  }

  public Process executeNonblocking( final File workingDir,
      final Writer out, final String... args ) throws IOException {
    Process proc = doExecute( workingDir, args );
    redirect( new InputStreamReader(proc.getInputStream()), out );
    return proc;
  }

  private Process doExecute( final File workingDir, final String... args )
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
