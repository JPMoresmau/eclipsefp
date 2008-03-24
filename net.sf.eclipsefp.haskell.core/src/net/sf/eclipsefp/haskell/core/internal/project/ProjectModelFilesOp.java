// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.project;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;

public class ProjectModelFilesOp implements IProjectCreationOperationExtraOp {

  private static final String ENC = "UTF-8"; //$NON-NLS-1$
  private static final String EXT_CABAL = "cabal"; //$NON-NLS-1$
  private static final String SETUP_HS = "Setup.hs"; //$NON-NLS-1$
  private static final String NL = "\n"; //$NON-NLS-1$


  // interface methods of IProjectCreationOperationExtraOp
  ////////////////////////////////////////////////////////

  public void run( final IProject project,
                   final IProgressMonitor mo ) throws CoreException {
    String name = project.getName();
    createFile( project, new Path( SETUP_HS ), getSetupFileContent(), mo );
    IPath cabalFile = new Path( name ).addFileExtension( EXT_CABAL );
    createFile( project, cabalFile, getCabalFileContent( name ), mo  );
  }


  // helping methods
  //////////////////

  private String getCabalFileContent( final String name ) {
    return   "Name:           " + name + NL //$NON-NLS-1$
           + "Version:        0.1 \n" //$NON-NLS-1$
           + "Hs-Source-Dirs: src\n"; //$NON-NLS-1$
  }

  private void createFile( final IProject project,
                           final IPath fileName,
                           final String content,
                           final IProgressMonitor mo ) throws CoreException {
    try {
      IProgressMonitor monitor = new SubProgressMonitor( mo, 1 );
      IFile file = project.getFile( fileName );
      InputStream is = new ByteArrayInputStream( content.getBytes( ENC ) );
      file.create( is, true, monitor );
    } catch( UnsupportedEncodingException uex ) {
      HaskellCorePlugin.log( uex );
    }
  }

  private String getSetupFileContent() {
    return "import Distribution.Simple\nmain = defaultMain\n"; //$NON-NLS-1$
  }
}