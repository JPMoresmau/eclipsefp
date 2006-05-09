// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.compiler;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.common.core.util.StreamRedirect;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;

/** <p>The abstract superclass for compilers.</p>
  * 
  * <p>Extend this class to create an FP compiler. You only have to provide
  * specific things like the command line for your compiler, the rest is 
  * handled automatically.</p> 
  * 
  * @author Leif Frenzel
  */
public abstract class AbstractHaskellCompiler implements IHaskellCompiler {

  public ICompilerOutput compile( final IFile file ) {
    List excList = new ArrayList();
    StringWriter output = new StringWriter();
    StringWriter errors = new StringWriter();
    int status = 0;
    try {
      Process proc = createProcess( file );
      StreamRedirect outRedirect = new StreamRedirect( "output_redirect", 
                                                       proc.getInputStream(), 
                                                       output );
      StreamRedirect errRedirect = new StreamRedirect( "error_redirect", 
                                                       proc.getErrorStream(), 
                                                       errors );
      outRedirect.start();
      errRedirect.start();
      status = proc.waitFor(); // wait for compiler to finish
      outRedirect.join();      // wait until out stream content is redirected
      errRedirect.join();      // wait until err stream content is redirected
      collectExceptions( excList, outRedirect, errRedirect );      
    } catch( Exception e ) {
      excList.add( e );
    } finally {
      output.flush();
      errors.flush();         
    }
    return createResult( excList, output, errors, status );
  }


  // helping methods
  //////////////////

  private Process createProcess( final IFile file ) throws IOException {
    IProject project = file.getProject();
    IHaskellProject hsProject = HaskellProjectManager.get( project ); 
    IPath src = project.getLocation().append( hsProject.getSourcePath() );
    File workDir = new File( src.toOSString() );
    String[] cmdLine = buildCommandLine( file, hsProject );
    String[] env = null; // inherit the environment from parent process
    return Runtime.getRuntime().exec( cmdLine, env, workDir );
  }

  private ICompilerOutput createResult( final List exList,
                                        final StringWriter out,
                                        final StringWriter err,
                                        final int status ) {
    return new CompilerOutput( status, out.toString(), err.toString(), exList );
  }

  private void collectExceptions( final List excList,
                                  final StreamRedirect outRedirect,
                                  final StreamRedirect errRedirect ) {
    if( outRedirect.getException() != null ) { 
      excList.add( outRedirect.getException() );
    }
    if( errRedirect.getException() != null ) { 
      excList.add( errRedirect.getException() );
    }
  }
}