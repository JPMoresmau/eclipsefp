// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.code;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import net.sf.eclipsefp.haskell.core.internal.code.CodeGenerator;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;


/** <p>helper to generate the code in the new module.</p>
  *
  * @author Leif Frenzel
  */
public class SourceFileGenerator {

  private final CodeGenerator fCodeGenerator;

  public SourceFileGenerator( final CodeGenerator codeGenerator ) {
    fCodeGenerator = codeGenerator;
  }


  public SourceFileGenerator() {
    this(new CodeGenerator());
  }


  /** Creates the new type using the specified information values. */
  public IFile createFile( final IProgressMonitor monitor,
                                  final ModuleCreationInfo info )
                                                          throws CoreException {
    if (monitor!=null){
      monitor.beginTask( CoreTexts.sourceFileGenerator_creating, 12 );
    }
    IContainer destFolder = createFolders( info, monitor );   // (6)
    IFile result = createFile( info, destFolder, monitor );   // (4)
    refresh( info, monitor );         // (2)
    if (monitor!=null){
      monitor.done();
    }
    return result;
  }


  // helping methods
  //////////////////

  private IContainer createFolders( final ModuleCreationInfo info,
                                           final IProgressMonitor monitor )
                                                          throws CoreException {
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
      if (!result.exists()){
        result.getLocation().toFile().mkdirs();
      }
    }
    return result;
  }

  private void refresh( final ModuleCreationInfo info,
                               final IProgressMonitor monitor )
                                                          throws CoreException {
    SubProgressMonitor refMon = monitor==null?null:new SubProgressMonitor( monitor, 2 );
    IContainer srcContainer = info.getSourceContainer();
    srcContainer.refreshLocal( IResource.DEPTH_INFINITE, refMon );
  }

  private IFile createFile( final ModuleCreationInfo info,
                                   final IContainer destFolder,
                                   final IProgressMonitor monitor )
                                                          throws CoreException {
    final String[] segments = getPathSegments( info );
    final String moduleName = info.getModuleName();
    final EHaskellCommentStyle style = info.getCommentStyle();
    String fileContent = fCodeGenerator.createModuleContent( segments,
        moduleName,
        style );
    String fileName = createFileName( style, moduleName );
    IFile result = destFolder.getFile( new Path( fileName ) );
    InputStream isContent = new ByteArrayInputStream( fileContent.getBytes() );
    SubProgressMonitor subMon = monitor==null?null:new SubProgressMonitor( monitor, 4 );
    result.create( isContent, true, subMon );
    return result;
  }

  private static String[] getPathSegments( final ModuleCreationInfo info ) {
    IPath path = info.getFolders();
    return ( path == null || !info.isFoldersQualify() ) ? new String[ 0 ] : path.segments();
  }

  private static String createFileName(final EHaskellCommentStyle style, final String moduleName ) {
    return moduleName + "." + style.getFileExtension(); //$NON-NLS-1$
  }
}