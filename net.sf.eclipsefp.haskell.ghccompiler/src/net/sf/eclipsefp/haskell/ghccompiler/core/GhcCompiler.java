// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.PipedReader;
import java.io.PipedWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.DefaultHaskellCompiler;
import net.sf.eclipsefp.haskell.core.internal.util.MultiplexedWriter;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import net.sf.eclipsefp.haskell.ghccompiler.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.IProcessRunner;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

/**
 * <p>
 * Implements the compiler specific parts for the Glasgow Haskell compiler
 * (ghc).
 * </p>
 *
 * @author Leif Frenzel
 */
public class GhcCompiler extends DefaultHaskellCompiler {

  private static boolean trace = GhcCompilerPlugin.isTracing();

  private final IProcessRunner fProcessRunner;
  private final IGhcOutputProcessor fOutputProcessor;
  private final CompilerParams compilerParams = new CompilerParams();

  public GhcCompiler() {
    fProcessRunner = new ProcessRunner();
    fOutputProcessor = new GhcOutputProcessor( );
  }

  public GhcCompiler(final IProcessRunner processRunner, final IGhcOutputProcessor outputProcessor) {
    fProcessRunner = processRunner;
    fOutputProcessor = outputProcessor;
  }

  @Override
  public void compile( final IFile file, final Writer outputWriter ) {
    final IProject project = file.getProject();
    String[] cmdLine = buildCommandLine( file, project );
    File workDir = null;
    for(IContainer c:ResourceUtil.getSourceFolders( project ) ) {
      IPath src = c.getFullPath();
      if( src.isPrefixOf( file.getFullPath() ) ) {
        workDir = new File( src.toOSString() );
      }
    }
    if( workDir == null ) {
      throw new IllegalStateException();
    }

    PipedWriter pipedWriter = new PipedWriter();
    PipedReader pipedReader;
    try {
      pipedReader = new PipedReader(pipedWriter);
    } catch( IOException ex ) {
      GhcCompilerPlugin.log( UITexts.error_processOutput, ex );
      return;
    }
    Writer out = new MultiplexedWriter( outputWriter, pipedWriter );
    StringWriter sw=new StringWriter();
    BufferedWriter err=new BufferedWriter( sw);
      try {

        fProcessRunner.executeNonblocking( workDir, out, err, cmdLine );
      } catch( IOException ex ) {
        GhcCompilerPlugin.log( UITexts.error_launchGhc, ex );
        return;
      }

      fOutputProcessor.setWorkingDir( workDir );
      GhcOutputParser outputParser = new GhcOutputParser( pipedReader, fOutputProcessor );
      try {
        outputParser.parse();
      } catch( IOException ex ) {
        GhcCompilerPlugin.log( UITexts.error_processOutput, ex );
      }
      try {
        err.flush();
        String errS=sw.toString();
        if (errS.length()>0){
          outputWriter.write( errS );
        }
      } catch (IOException ioe){
        GhcCompilerPlugin.log( UITexts.error_processError, ioe );
      }
  }

  private String[] buildCommandLine( final IFile file,
      final IProject project ) {
    if( trace ) {
      System.out.println( "Constructing command line for file " + file ); //$NON-NLS-1$
    }

    String outDir = project.getFolder( "bin" ).getFullPath().toOSString();//getAbsPath( project, haskellProject.getOutputPath() ); //$NON-NLS-1$

    List<String> cmdLine = new ArrayList<String>();
    // command and special options
    cmdLine.add( CompilerManager.getCompilerExecutable() );
    String libPath = Util.constructLibPath( file);
    if( !libPath.equals( "" ) ) { //$NON-NLS-1$
      cmdLine.add( libPath );
    }
    cmdLine.add( "--make" ); //$NON-NLS-1$
    cmdLine.add( "-odir" ); //$NON-NLS-1$
    cmdLine.add( outDir );
    cmdLine.add( "-hidir" ); //$NON-NLS-1$
    cmdLine.add( outDir );
    cmdLine.add( "-ferror-spans" ); //$NON-NLS-1$

    /*cmdLine.add( "-o" ); //$NON-NLS-1$
    IPath targetName = getTargetName( haskellProject );
    if( targetName.segmentCount() > 1 ) {
      try {
        ResourceUtil.mkdirs( targetName.removeLastSegments( 1 ), project );
      } catch( final CoreException cex ) {
        String msg = "Could not create folders for target executable"; //$NON-NLS-1$
        GhcCompilerPlugin.log( msg, cex );
      }
    }*/
    //cmdLine.add( project.getLocation().append( targetName ).toOSString() );
    cmdLine.addAll( compilerParams.construct(null) );
    cmdLine.add( ResourceUtil.getSourceFolderRelativeName( file ).toOSString() );
    if( trace ) {
      HaskellCorePlugin.dump( cmdLine );
    }
    return cmdLine.toArray( new String[ cmdLine.size() ] );
  }

  // helping methods
  // ////////////////

//  private String getAbsPath( final IProject project, final IPath path ) {
//    return project.getLocation().toOSString() + File.separator
//        + path.toOSString();
//  }
//
//  private IPath getTargetName( final IHaskellProject haskellProject ) {
//    String result = "theResult"; //$NON-NLS-1$
//    Set<IPath> targetNames = haskellProject.getTargetNames();
//    if( targetNames.size() > 0 ) {
//      result = targetNames.iterator().next().toOSString();
//    }
//    return new Path( result );
//  }
}