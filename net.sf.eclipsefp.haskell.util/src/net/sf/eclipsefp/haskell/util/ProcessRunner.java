package net.sf.eclipsefp.haskell.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ProcessRunner implements IProcessRunner {

//  private final IProcessFactory fProcessFactory;
  
  private final static String STDOUT_REDIRECT = "output_redirect";
  private final static String STDERR_REDIRECT = "error_redirect";

  public static final String LIBRARY_VERSION="using version ((\\d|\\.)*) of the Cabal library";
	
  public ProcessRunner() {
    //this( new ProcessFactory() );
  }

//  public ProcessRunner( final IProcessFactory factory ) {
//    fProcessFactory = factory;
//  }

  public int executeBlocking( final File workingDir, final Writer out,
      final Writer err, final String ... args ) throws IOException {

    Process proc = doExecute( workingDir,err==null, args );

    Thread outRedirect = redirect( new InputStreamReader( proc.getInputStream() ), out, STDOUT_REDIRECT );
    Thread errRedirect = null;
    if (err!=null){
    	errRedirect = redirect( new InputStreamReader( proc.getErrorStream() ), err, STDERR_REDIRECT );
    }
    int code=-1;
    try {
      code=proc.waitFor(); // wait for process to finish
      outRedirect.join(); // wait until out stream content is redirected
      if (errRedirect!=null){
    	  errRedirect.join(); // wait until err stream content is redirected
      }
    } catch (InterruptedException ex) {
      // ignore
    }
    return code;
  }

  public Process executeNonblocking( final File workingDir, final Writer out,
      Writer err, final String ... args ) throws IOException {
    Process proc = doExecute( workingDir,err==null, args );
    redirect( new InputStreamReader( proc.getInputStream() ), out, STDOUT_REDIRECT );
    if (err==null){
    	err=new StringWriter();
    }
    redirect( new InputStreamReader( proc.getErrorStream() ), err, STDERR_REDIRECT );
    
    return proc;
  }

  private Process doExecute( final File workingDir, final boolean redirect,final String ... args )
      throws IOException {
    //Process proc = fProcessFactory.startProcess( workingDir, args );
	ProcessBuilder builder = new ProcessBuilder(args);
	builder.directory( workingDir );
	builder.redirectErrorStream(redirect);
    return builder.start();
  }

  private static Thread redirect( final Reader in, final Writer out, String name ) {
    Thread outRedirect = new StreamRedirect( name, in, out );
    outRedirect.start();
    return outRedirect;
  }
  
  public static Thread[] consume(Process proc){
	  Thread t1=redirect( new InputStreamReader( proc.getInputStream() ), new StringWriter(), STDOUT_REDIRECT );
	  Thread t2=redirect( new InputStreamReader( proc.getErrorStream() ), new StringWriter(), STDERR_REDIRECT );
	  return new Thread[]{t1,t2};
  }

  /**
   * get both the version of the executabl
   * @param path the path
   * @param wait should we wait for the executable or do we NOT trust it to return and shut down (older versions of scion-browser did not return on the --version flag
   * @return the version of the executable or null if we could not get it
   * @throws IOException
   */  
  public static String getExecutableVersion(String path,boolean wait) throws IOException{
	  List<String> ls=getExecutableAndCabalVersion(path, wait);
	  if (ls!=null && ls.size()>0){
		  return ls.get(0);
	  }
	  return null;
  }
  
  /**
   * get both the version of the executable and the version of the Cabal library used to build it
   * @param path the path
   * @param wait should we wait for the executable or do we NOT trust it to return and shut down (older versions of scion-browser did not return on the --version flag
   * @return a list, potentially null or empty, containing first the version of the executable, then the version of Cabal
   * @throws IOException
   */
  public static List<String> getExecutableAndCabalVersion(String path,boolean wait) throws IOException{
	  File f=new File(path);
	  if (f.exists()){
		  StringWriter sw=new StringWriter();
		  // get the process
		  Process p = new ProcessRunner().doExecute( f.getParentFile(), false,f.getAbsolutePath(),"--version" );
		  // redirect output into string writer
		  Thread t1= redirect( new InputStreamReader( p.getInputStream() ), sw, STDOUT_REDIRECT );
		  // ignore error
    	  redirect( new InputStreamReader( p.getErrorStream() ), new StringWriter(), STDERR_REDIRECT );

		  try {
			  // we trust the process to be short lived
			  if (wait){
				  try {
					  p.waitFor();
				  } catch (InterruptedException ie){
					  //
				  }
			  } else {
				  // we do not trust the process to be short lived
				  for (int a=0;a<200;a++){ // 200 * 100 -> 20 seconds maxi
					  try {
						  p.exitValue();
						  break;
					  } catch (IllegalThreadStateException ise){
						  // still running
					  }
					  try {
						  Thread.sleep(100);
					  } catch (InterruptedException ie){
						  //
					  }
				  }
			  }
		  } finally {
			  p.destroy();
		  }		  
		  // we wait for the redirect thread to finish reading/writing
		  try {
			  t1.join(); // wait for thread to finish correctly, old code was: 10*1000 10 seconds max waiting for write
		  } catch (InterruptedException ignore){
			  // noop
		  }
		  List<String> ret=new ArrayList<String>();
		  // now we should have the proper result
		  BufferedReader br=new BufferedReader(new StringReader(sw.toString().trim()));
		  String line=br.readLine();
		  if (line!=null){
			  int ix=line.lastIndexOf(' ');
			  if (ix>-1 && ix<line.length()){
				  line=line.substring(ix+1);
				  if (Character.isDigit(line.charAt(0))){
					  ret.add(line);
				  }
			  }
		  }
		  // we read the second line to see if we have the cabal version
		  line=br.readLine();
		  if (line!=null){
			  Matcher m=Pattern.compile(LIBRARY_VERSION).matcher(line);
			  if (m.matches()){
				  ret.add(m.group(1));
			  }
		  }
		  return ret;
	  }
	  return null;
  }
}
