// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.util;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;

/**
 * <p>
 * A thread that copies the contents of an input stream to an output stream and
 * terminates when the input stream is finished.
 * </p>
 *
 * @author Leif Frenzel
 */
public class StreamRedirect extends Thread {

  private static final int BUFFER_SIZE = 2048;

  private final Reader fInput;

  private Writer output;

  public StreamRedirect( final String name, final Reader in,
      final Writer out ) {
    super( name );
    fInput = in;
    output = out;
    setPriority( Thread.MAX_PRIORITY - 1 );
  }

  public StreamRedirect( final Reader in, final Writer out ) {
    this( "Stream redirect thread", in, out ); //$NON-NLS-1$
  }

  // interface methods of java.lang.Thread
  // //////////////////////////////////////

  @Override
  public void run() {
    char[] cbuf = new char[ BUFFER_SIZE ];
    int count;
    try {
      while( ( count = fInput.read( cbuf, 0, BUFFER_SIZE ) ) >= 0 ) {
        output.write( cbuf, 0, count );
        output.flush(); // flush straight away
      }
      
      output.close();
    } catch( IOException ex ) {
      // reading error, abort multiplexing
    }
  }

	public Writer getOutput() {
		return output;
	}
	
	public void setOutput(Writer output) {
		if (this.output!=null){
			 try {
				 this.output.flush();
				 this.output.close();
		    } catch( IOException ignore ) {
		      // noop
		    }
		}
		this.output = output;
	}
}

