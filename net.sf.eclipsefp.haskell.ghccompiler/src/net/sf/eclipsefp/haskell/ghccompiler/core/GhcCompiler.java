// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.compiler.DefaultHaskellCompiler;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

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

	private final CompilerParams compilerParams = new CompilerParams();

	private final IProcessRunner fProcessRunner;

	public GhcCompiler() {
		this(new ProcessRunner());
	}

	/**
	 * Constructor for testing
	 */
	public GhcCompiler(final IProcessRunner procRunner) {
		fProcessRunner = procRunner;
	}

	@Override
  public void compile( final IFile file, final Writer outputWriter ) {
    final IProject project = file.getProject();
    IHaskellProject hsProject = HaskellProjectManager.get( project );
    String[] cmdLine = buildCommandLine( file, hsProject );
    File workDir = null;
    Set<IPath> sourcePaths = hsProject.getSourcePaths();
    for( IPath sourcePath: sourcePaths ) {
      IPath src = project.getFullPath().append( sourcePath );
      if( src.isPrefixOf( file.getFullPath() ) ) {
        IPath loc = project.getFolder( sourcePath ).getLocation();
        workDir = new File( loc.toOSString() );
      }
    }
    if( workDir == null ) {
      throw new IllegalStateException();
    }
    String output = fProcessRunner.execute( workDir, outputWriter, cmdLine );
    // TODO replace by something not Cohatoe-based
    // TemporaryCompilerHelper.applyOutput( output, file );
  }

	private String[] buildCommandLine( final IFile file,
      final IHaskellProject haskellProject ) {
    if( trace ) {
      System.out.println( "Constructing command line for file " + file ); //$NON-NLS-1$
    }

    IProject project = haskellProject.getResource();
    String outDir = getAbsPath( project, haskellProject.getOutputPath() );

    List<String> cmdLine = new ArrayList<String>();
    // command and special options
    cmdLine.add( Util.getCompilerExecutable() );
    String libPath = Util.constructLibPath( haskellProject );
    if( !libPath.equals( "" ) ) { //$NON-NLS-1$
      cmdLine.add( libPath );
    }
    cmdLine.add( "--make" ); //$NON-NLS-1$
    cmdLine.add( "-odir" ); //$NON-NLS-1$
    cmdLine.add( outDir );
    cmdLine.add( "-hidir" ); //$NON-NLS-1$
    cmdLine.add( outDir );
    cmdLine.add( "-ferror-spans" ); //$NON-NLS-1$

    cmdLine.add( "-o" ); //$NON-NLS-1$
    IPath targetName = getTargetName( haskellProject );
    if( targetName.segmentCount() > 1 ) {
      try {
        ResourceUtil.mkdirs( targetName.removeLastSegments( 1 ), project );
      } catch( final CoreException cex ) {
        String msg = "Could not create folders for target executable"; //$NON-NLS-1$
        GhcCompilerPlugin.log( msg, cex );
      }
    }
    cmdLine.add( project.getLocation().append( targetName ).toOSString() );
    cmdLine.addAll( compilerParams.construct() );
    cmdLine.add( ResourceUtil.getSourceDirRelativeName( file, haskellProject ) );
    if( trace ) {
      HaskellCorePlugin.dump( cmdLine );
    }
    return cmdLine.toArray( new String[ cmdLine.size() ] );
  }

	// helping methods
	// ////////////////

	private String getAbsPath(final IProject project, final IPath path) {
		return project.getLocation().toOSString() + File.separator
				+ path.toOSString();
	}

	private IPath getTargetName( final IHaskellProject haskellProject ) {
    String result = "theResult"; //$NON-NLS-1$
	  Set<IPath> targetNames = haskellProject.getTargetNames();
    if( targetNames.size() > 0 ) {
      result = targetNames.iterator().next().toOSString();
	  }
    return new Path( result );
  }
}