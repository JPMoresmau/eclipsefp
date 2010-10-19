package net.sf.eclipsefp.haskell.scion.internal.servers;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.LinkedList;
import java.util.List;

import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.internal.util.Trace;
import net.sf.eclipsefp.haskell.scion.internal.util.ScionText;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

import org.eclipse.core.runtime.IPath;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

/**
 * Implementation of {@link ScionServer} using standard in/out to communicate with Scion
 * 
 * @author JP Moresmau
 */
public class StdStreamScionServer extends ScionServer {
  /** The input receiver Job */
  private InputReceiver         inputReceiver;
  /** scion-server response prefix */
  private static final String PREFIX = "scion:";

  public StdStreamScionServer(String projectName, IPath serverExecutable, Writer serverOutput, File directory) {
    super(projectName, serverExecutable, serverOutput, directory);
  }

  protected synchronized void doStartServer(String projectName) throws ScionServerStartupException {
    List<String> command = new LinkedList<String>();
    command.add(serverExecutable.toOSString());
    command.add("-i ");

    // Launch the process
    ProcessBuilder builder = new ProcessBuilder(command);
    if (directory != null && directory.exists()) {
      builder.directory(directory);
    }
    builder.redirectErrorStream(true);
    try {
      process = builder.start();
    } catch (Throwable ex) {
      throw new ScionServerStartupException(ScionText.scionServerCouldNotStart_message, ex);
    }
    // Connect to the process's stdout to capture messages
    // Assume that status messages and such will be UTF8 only
    try {
      serverOutStream = new BufferedReader(new InputStreamReader(process.getInputStream(), "UTF8"));
      serverInStream = new BufferedWriter(new OutputStreamWriter(process.getOutputStream(), "UTF8"));

      String receiverName = getClass().getSimpleName() + "/" + projectName;
      inputReceiver = new InputReceiver(receiverName);
      inputReceiver.start();
    } catch (UnsupportedEncodingException ex) {
      // make compiler happy, because UTF8 is always supported
    }
  }
  
  @Override
  public void doStopServer() {
    if (inputReceiver != null) {
      // Stop the input receiver.
      try {
        inputReceiver.setTerminate();
        inputReceiver.interrupt();
        inputReceiver.join();
      } catch (InterruptedException e) {
        // Nothing to do.
      }
    }
  }

  /** The input receiver thread.
   *
   * This thread reads the scion-server's standard input and error, looking for input lines that start with
   * {@link StdStreamScionServer#PREFIX}. Matching lines are sent to {@link ScionServer#processResponse(JSONObject)}.
   *  All input is echoed to the {@link ScionServer#serverOutput} output stream.
   */
  public class InputReceiver extends Thread {
    private boolean terminateFlag;
    
    public InputReceiver(String name) {
      super(name);
      terminateFlag = false;
    }
    
    public void setTerminate() {
      terminateFlag = true;
    }

    @Override
    public void run() {
      final String fromServer = projectName + "/" + FROM_SERVER_PREFIX;
      while (!terminateFlag && serverOutStream != null) {
        JSONObject response;
        // long t0=System.currentTimeMillis();
        try {
          String responseString = serverOutStream.readLine();
          if (responseString != null) {
            if (responseString.startsWith(PREFIX)) {
              response = new JSONObject(new JSONTokener(responseString.substring(PREFIX.length())));

              Trace.trace(fromServer, "%s", response.toString());
              serverOutput.write(FROM_SERVER_PREFIX.concat(response.toString()).concat(PlatformUtil.NL));
              serverOutput.flush();
              
              processResponse(response);
            } else {
              serverOutput.write(responseString + PlatformUtil.NL);
              serverOutput.flush();
            }
          } else {
            serverOutput.write(ScionText.scionServerGotEOF_message + PlatformUtil.NL);
            serverOutput.flush();
            Trace.trace(fromServer, ScionText.scionServerGotEOF_message);
            stopServer();
          }
        } catch (JSONException ex) {
          try {
            serverOutput.write(ScionText.scionJSONParseException_message + PlatformUtil.NL);
            ex.printStackTrace(new PrintWriter(serverOutput));
            serverOutput.flush();

            ScionPlugin.logError(ScionText.scionJSONParseException_message, ex);
            
            stopServer();
          } catch (IOException ex2) {
            ScionPlugin.logError(ScionText.scionServerNotRunning_message, ex2);
            stopServer();
          }
        } catch (IOException ex) {
          ScionPlugin.logError(ScionText.scionServerNotRunning_message, ex);
        }
        // long t1=System.currentTimeMillis();
        // System.err.println("receive+parse:"+(t1-t0));
      }
    }
  }
}
