package net.sf.eclipsefp.haskell.util;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.LinkedList;


  /**
   * A separate thread to write the communication with the server see
   * https://bugs.eclipse.org/bugs/show_bug.cgi?id=259107
   */
public abstract class OutputWriter extends Thread {
	    /** the message list **/
	    private final LinkedList<String> messages   = new LinkedList<String>();
	    /** should we stop? **/
	    private boolean                  terminateFlag;

	    private Writer serverOutput;
	    
	    public OutputWriter(String name,Writer output) {
	      super(name);
	      this.serverOutput=output;
	    }

	    public void setTerminate() {
	      terminateFlag = true;
	      interrupt();
	    }

	    public void addMessage(String msg) {
	      synchronized (messages) {
	        messages.add(msg);
	        messages.notify();
	      }
	    }

	    public void addMessage(char[] buf, int start, int length) {
	      synchronized (messages) {
	        messages.add(new String(buf, start, length));
	        messages.notify();
	      }
	    }

	    public void addMessage(Exception e) {
	      StringWriter sw = new StringWriter();
	      PrintWriter pw = new PrintWriter(sw);
	      e.printStackTrace(pw);
	      pw.flush();
	      synchronized (messages) {
	        messages.add(sw.toString());
	        messages.notify();
	      }
	    }

	    @Override
	    public void run() {
	      while (!terminateFlag && serverOutput != null) {
	        String m = null;
	        synchronized (messages) {
	          try {
	            while (messages.isEmpty()) {
	              messages.wait();
	            }

	          } catch (InterruptedException ignore) {
	            // noop
	          }
	          if (!messages.isEmpty()) {
	            m = messages.removeFirst();
	          }
	        }
	        if (m != null) {
	          try {
	            final int mLen = m.length();
	            int i = 0;

	            // Break the message up into 1K chunks for better UI responsiveness, since this all is going to
	            // an IOConsole.
	            while ( mLen - i > 1024 ) {
	              serverOutput.write(m, i, 1024);
	              serverOutput.flush();
	              i += 1024;
	            }
	            serverOutput.write(m, i, mLen - i);
	            serverOutput.write(PlatformUtil.NL);
	            serverOutput.flush();
	          } catch (IOException ex) {
	            if (!terminateFlag) {
	              onIOError(ex);
	            }
	          } catch (Throwable se) {
	            // probably device has been disposed
	            if (!terminateFlag) {
	            	onThrowable(se);
	            }
	          }
	        }
	      }
	    }
	  
	    public abstract void onIOError(IOException ex);
	    public abstract void onThrowable(Throwable se);
	    
}
