// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.core.util;

import java.io.*;

/** <p>A thread that copies the contents of an input stream to an output 
  * stream and terminates when the input stream is finished.</p>
  * 
  * @author Leif Frenzel
  */
public class StreamRedirect extends Thread {

  private static final int BUFFER_SIZE = 2048;

  private Reader in;
  private Writer out;
  private Exception ex;
  
  
  public StreamRedirect( final String name, 
                         final InputStream in, 
                         final Writer out ) {
    super( name );
    this.in = new InputStreamReader( in );
    this.out = out;
    setPriority( Thread.MAX_PRIORITY - 1 );
  }

  public Exception getException() {
    return ex;
  }


  // interface methods of java.lang.Thread
  ////////////////////////////////////////

  public void run() {
    try {
      char[] cbuf = new char[ BUFFER_SIZE ];
      int count;
      while( ( count = in.read( cbuf, 0, BUFFER_SIZE ) ) >= 0 ) {
        out.write( cbuf, 0, count );
        out.flush();
      }
    } catch( IOException ex ) {
      this.ex = ex;
    }
  }
}