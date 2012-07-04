/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author JP Moresmau
 *
 */
public class BinaryStreamRedirect  extends Thread {

	  private static final int BUFFER_SIZE = 2048;

	  private final InputStream fInput;

	  private OutputStream output;

	  public BinaryStreamRedirect( final String name, final InputStream in,
	      final OutputStream out ) {
	    super( name );
	    fInput = in;
	    output = out;
	    setPriority( Thread.MAX_PRIORITY - 1 );
	  }

	  public BinaryStreamRedirect( final InputStream in, final OutputStream out ) {
	    this( "Stream redirect thread", in, out ); //$NON-NLS-1$
	  }

	  // interface methods of java.lang.Thread
	  // //////////////////////////////////////

	  @Override
	  public void run() {
	    byte[] cbuf = new byte[ BUFFER_SIZE ];
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

		public OutputStream getOutput() {
			return output;
		}
		
		public void setOutput(OutputStream output) {
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
