// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Scanner;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;


/**
 * <p>
 * helper to generate the code in the new module from a template.
 * </p>
 *
 * @author Leif Frenzel
 * @author Alejandro Serrano
 */
public class TemplateFileGenerator {

  public static String MODULE_REPLACEMENT = "${module}";

  String contents;
  String fileExtension;

  public TemplateFileGenerator(final String contents, final String extension) {
    this.contents = contents;
    this.fileExtension = extension;
  }

  public TemplateFileGenerator(final InputStream stream, final String extension) {
    @SuppressWarnings("resource")
    Scanner scanner = new Scanner(stream);
    this.contents = scanner.useDelimiter("\\Z").next();
    this.fileExtension = extension;
  }


  /** Creates the new type using the specified information values. */
  public IFile createFile( final IProgressMonitor monitor,
      final ModuleCreationInfo info ) throws CoreException {
    if( monitor != null ) {
      monitor.beginTask( UITexts.creating_file, 12 );
    }
    IContainer destFolder = createFolders( info, monitor ); // (6)
    IFile result = createFile( info, destFolder, monitor ); // (4)
    refresh( info, monitor ); // (2)
    if( monitor != null ) {
      monitor.done();
    }
    return result;
  }


  // helping methods
  // ////////////////

  private IContainer createFolders( final ModuleCreationInfo info,
      final IProgressMonitor monitor ) throws CoreException {
    IPath foldersPath = info.getFolders();
    IContainer sourceContainer = info.getSourceContainer();
    IContainer result = null;
    if( foldersPath != null && foldersPath.segmentCount() > 0 ) {
      String[] segments = foldersPath.segments();
      IContainer folder = sourceContainer;
      for( int i = 0; i < segments.length; i++ ) {
        IPath path = new Path( segments[ i ] );
        folder = folder.getFolder( path );
        if( !folder.exists() && folder instanceof IFolder ) {
          SubProgressMonitor subMon = new SubProgressMonitor( monitor, 1 );
          ( ( IFolder )folder ).create( false, true, subMon );
        }
      }
      result = folder;
    } else {
      result = sourceContainer;
    }
    return result;
  }

  private void refresh( final ModuleCreationInfo info,
      final IProgressMonitor monitor ) throws CoreException {
    SubProgressMonitor refMon = monitor == null ? null
        : new SubProgressMonitor( monitor, 2 );
    IContainer srcContainer = info.getSourceContainer();
    srcContainer.refreshLocal( IResource.DEPTH_INFINITE, refMon );
  }

  private IFile createFile( final ModuleCreationInfo info,
      final IContainer destFolder, final IProgressMonitor monitor )
      throws CoreException {
    //final String[] segments = getPathSegments( info );
    final String moduleName = info.getModuleName();
    //final EHaskellCommentStyle style = info.getCommentStyle();
    String fileContent = contents.replace( MODULE_REPLACEMENT, info.getQualifiedModuleName() );
    String fileName = createFileName( moduleName );
    IFile result = destFolder.getFile( new Path( fileName ) );
    InputStream isContent = new ByteArrayInputStream( fileContent.getBytes() );
    SubProgressMonitor subMon = monitor == null ? null
        : new SubProgressMonitor( monitor, 4 );
    result.create( isContent, true, subMon );
    return result;
  }

//  private static String[] getPathSegments( final ModuleCreationInfo info ) {
//    IPath path = info.getFolders();
//    return ( path == null ) ? new String[ 0 ] : path.segments();
//  }

  private String createFileName( final String moduleName ) {
    return moduleName + "." + fileExtension; //$NON-NLS-1$
  }
}