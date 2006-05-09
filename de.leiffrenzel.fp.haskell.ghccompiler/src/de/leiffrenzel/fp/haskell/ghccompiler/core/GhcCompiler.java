// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ghccompiler.core;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

import net.sf.eclipsefp.haskell.core.compiler.AbstractHaskellCompiler;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.util.TracingUtil;
import de.leiffrenzel.fp.haskell.ghccompiler.GhcCompilerPlugin;

/** <p>Implements the compiler specific parts for the Glasgow Haskell compiler
  * (ghc).</p>
  * 
  * @author Leif Frenzel
  */
public class GhcCompiler extends AbstractHaskellCompiler {

  private static boolean trace = GhcCompilerPlugin.isTracing();
  
  private CompilerParams compilerParams = new CompilerParams();
  
  
  // interface methods of AbstractHaskellCompiler
  ///////////////////////////////////////////////
  
  public String[] buildCommandLine( final IFile file,
                                    final IHaskellProject haskellProject ) {
    if( trace ) {
      System.out.println( "Constructing command line for file " + file );
    }
    
    IProject project = haskellProject.getResource();
    String outDir = getAbsPath( project, haskellProject.getOutputPath() );
    
    ArrayList cmdLine = new ArrayList();
    //command and special options
    cmdLine.add( Util.getCompilerExecutable() );
    cmdLine.add( "--make" );
    String libPath = Util.constructLibPath( haskellProject );
    if( !libPath.equals( "" ) ) { 
      cmdLine.add( libPath );
    }
    cmdLine.add( "-odir" );
    cmdLine.add( outDir );
    cmdLine.add( "-hidir" );
    cmdLine.add( outDir );
    
    String binDir = getAbsPath( project, haskellProject.getBinPath() );
    cmdLine.add( "-o" );
    cmdLine.add( binDir + File.separator + getTargetName( haskellProject ) );
    cmdLine.addAll( compilerParams.construct() );
    cmdLine.add( getFileName( file, haskellProject ) );
    if( trace ) {
      TracingUtil.dump( cmdLine );
    }
    return toArray( cmdLine );
  }

  
  // helping methods
  //////////////////
  
  private String getAbsPath( final IProject project, final IPath path ) {
    return   project.getLocation().toOSString()
           + File.separator 
           + path.toOSString();
  }

  private String getTargetName( final IHaskellProject haskellProject ) {
    String targetName = haskellProject.getTargetName();
    if( targetName.equals( "" ) ) {
      targetName = "theResult";
    }
    return targetName;
  }

  private String getFileName( final IFile file, 
                              final IHaskellProject haskellProject ) {
    IPath sourcePath = haskellProject.getSourcePath();
    return getSourceRelPath( file, sourcePath ).toOSString();
  }
  
  private IPath getSourceRelPath( final IFile file, final IPath sourcePath ) {
    IPath projectRelPath = file.getProjectRelativePath(); 
    int num = projectRelPath.matchingFirstSegments( sourcePath );
    return projectRelPath.removeFirstSegments( num );
  }
  
  private final String[] toArray( final List list ) {
    String[] result = new String[ list.size() ];
    list.toArray( result );
    return result;
  }
}