package net.sf.eclipsefp.haskell.core.partitioned.uuagc;

import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.partitioned.runner.ProcessorError;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

/**
 * UUAGC builder: calls UUAGC, parses the results and shows
 * them as file markers in the editor. The .hs files
 * created are marked as derived.
 *
 * @author Alejandro Serrano
 */
public class UuagcBuilder extends IncrementalProjectBuilder {

  public static String BUILDER_ID = UuagcBuilder.class.getName();

  public UuagcBuilder() {
    // Do nothing
  }

  @Override
  protected IProject[] build( final int kind, final Map args,
      final IProgressMonitor monitor ) throws CoreException {
    if( kind == INCREMENTAL_BUILD || kind == AUTO_BUILD ) {
      // Get delta
      final IResourceDelta delta = getDelta( getProject() );
      if( delta == null ) {
        return null;
      }

        delta.accept( new DeltaVisitor() );

    } else if( kind == CLEAN_BUILD ) {
         clean( monitor );
    } else if( kind == FULL_BUILD ) {

        clean( monitor );
         getProject().accept( new FullBuildVisitor() );


    }
    // Complete code here
    return null;
  }


  @Override
  public ISchedulingRule getRule( final int kind, final Map<String, String> args ) {
    // prevent other project operations, but operations elsewhere in the workspace are fine
    return getProject();
  }

  @Override
  protected void clean( final IProgressMonitor monitor ) throws CoreException {
    getProject().accept( new CleanVisitor() );
  }

  static boolean mustBeVisited( final IResource resource ) {
    return ( resource instanceof IFile
        && hasCorrectExtension( resource.getProjectRelativePath() ) && ResourceUtil.isInSourceFolder( ( IFile )resource ) );
  }


  static boolean hasCorrectExtension( final IPath path ) {
    if( path == null ) {
      return false;
    }
    String extension = path.getFileExtension();
    if( extension == null ) {
      return false;
    }
    return extension.equals( FileUtil.EXTENSION_UUAGC );
  }

  static void createMarker( final IResource resource, final ProcessorError e )
      throws CoreException {
    int colonPos = e.getMessage().indexOf( ':' );
    String severityMsg = e.getMessage().substring( 0, colonPos ).trim();
    String theMessage = e.getMessage().substring( colonPos + 1 ).trim();

    IMarker marker = resource
        .createMarker( HaskellCorePlugin.ID_UUAGC_MARKER );
    marker.setAttribute( IMarker.MESSAGE, theMessage );
    marker.setAttribute( IMarker.SEVERITY,
            severityMsg.equals( "error" ) ? IMarker.SEVERITY_ERROR : IMarker.SEVERITY_WARNING); //$NON-NLS-1$
    marker.setAttribute( IMarker.LINE_NUMBER, e.getLine() );
  }

}
