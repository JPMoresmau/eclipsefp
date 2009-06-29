package net.sf.eclipsefp.haskell.ghccompiler.core;

import java.io.File;
import java.io.IOException;
import net.sf.eclipsefp.haskell.scion.types.Note;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;

/**
 * Listens to a GHC output parser and applies
 * error and warning markers to the appropriate files based on the output.
 *
 * @author Thomas ten Cate
 */
public class GhcOutputProcessor implements IGhcOutputListener {

  private final File workingDir;

  public GhcOutputProcessor(final File workingDir) {
    this.workingDir = workingDir;
  }

  public void compiling( final String fileName, final int number, final int total ) {
    IFile file = findFile(fileName);
    try {
      file.deleteMarkers( IMarker.PROBLEM, true, IResource.DEPTH_INFINITE );
    } catch( CoreException ex ) {
      // too bad!
    }
  }

  public void message( final Note note ) {
    IFile file = findFile( note.getLocation().getFileName() );
    try {
      note.applyAsMarker( file );
    } catch( CoreException ex ) {
      // too bad!
    }
  }

  private IFile findFile( final String fileName ) {
    File absFile = makeAbsolute( fileName );
    return ResourcesPlugin.getWorkspace().getRoot().getFileForLocation( new Path(absFile.getPath()) );
  }

  private File makeAbsolute(final String fileName) {
    File file = new File(workingDir, fileName);
    try {
      file = file.getCanonicalFile();
    } catch (IOException ex) {
      // then just return the original
    }
    return file;
  }

}
