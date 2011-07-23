package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Map;
import java.util.Random;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

/**
 * launch delegate for Haskell executables executables
 *
 * @author JP Moresmau
 *
 */
public class ExecutableProfilingHaskellLaunchDelegate extends
    ExecutableOrTestSuiteHaskellLaunchDelegate {

  final String HP_EXTENSION = "hp"; //$NON-NLS-1$
  final String RUNTIME_OPTIONS = "+RTS -hT"; //$NON-NLS-1$
  final String DATETIME_FORMAT = "yyMMdd.HHmmss"; //$NON-NLS-1$

  String previousFilename = null;
  String savedFilename = null;

  @Override
  protected String getExtraArguments() {
    return RUNTIME_OPTIONS;
  }

  @Override
  protected void preProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch,
      final Map<String, String> processAttribs ) {
    // NOOP

  }

  @Override
  protected void postProcessCreation( final ILaunchConfiguration configuration,
      final String mode, final ILaunch launch, final IProcess process ) {
    // NOOP
  }

  @Override
  protected void preProcessDefinitionCreation(
      final ILaunchConfiguration configuration, final String mode, final ILaunch launch ) {
    // Save previous *.hp files that may be overwritten
    try {
      File workingDir = determineWorkingDir( configuration );
      IPath exeLocation = getExecutableLocation( configuration );
      String hpFilename = exeLocation.removeFileExtension()
          .addFileExtension( HP_EXTENSION ).lastSegment();
      File fileToSave = new File( workingDir.getAbsolutePath() + File.separator + hpFilename );
      previousFilename = fileToSave.getAbsolutePath();
      if (fileToSave.exists()) {
        // We need to save the previous file
        Random r = new Random( System.currentTimeMillis() );
        int n = r.nextInt();
        savedFilename = previousFilename + "." + n; //$NON-NLS-1$
        fileToSave.renameTo( new File(savedFilename) );
      }
    } catch (Exception e) {
      savedFilename = null;
    }
  }

  @Override
  protected void postProcessFinished() {
    Calendar cal = Calendar.getInstance( );
    SimpleDateFormat sdf = new SimpleDateFormat( DATETIME_FORMAT );
    String datetime = sdf.format( cal.getTime( ) );
    Path prevPath = new Path( previousFilename );
    IPath newPath = prevPath.removeFileExtension().addFileExtension( datetime )
        .addFileExtension( HP_EXTENSION );
    String newFilename = newPath.toOSString();

    // Rename newly created file
    File prevFile = new File( previousFilename );
    File newFile = new File( newFilename );
    prevFile.renameTo( newFile );

    // Rename previous file
    if (savedFilename != null) {
      prevFile = new File( previousFilename ); // Just in case...
      newFile = new File( savedFilename );
      newFile.renameTo( prevFile );
    }

    final File fileToOpen = new File( newFilename );

    Display.getDefault().syncExec( new Runnable() {

      public void run() {
        try {
          if( fileToOpen.exists() && fileToOpen.isFile() ) {
            IFileStore fileStore = EFS.getLocalFileSystem().getStore(
                fileToOpen.toURI() );
            IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
            IDE.openEditorOnFileStore( page, fileStore );
          } else {
            // Do something if the file does not exist
          }
        } catch( Exception e ) {
          // Do nothing
        }
      }
    } );
  }

}
