package net.sf.eclipsefp.haskell.core.partitioned.happy;

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
 * Happy builder: calls Happy, parses the results and shows
 * them as file markers in the editor. The .hs files
 * created are marked as derived.
 *
 * @author Alejandro Serrano
 */
public class HappyBuilder extends IncrementalProjectBuilder {

  public static String BUILDER_ID = HappyBuilder.class.getName();

  public HappyBuilder() {
    // Do nothing
  }

  @Override
  protected IProject[] build( final int kind,
      @SuppressWarnings("rawtypes") final Map args,
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
  protected void clean( final IProgressMonitor monitor ) throws CoreException {
    getProject().accept( new CleanVisitor() );
  }

  static boolean mustBeVisited( final IResource resource ) {
    return ( resource instanceof IFile
        && hasCorrectExtension( resource.getProjectRelativePath() ) && ResourceUtil.isInSourceFolder( ( IFile )resource ) );
  }

  @Override
  public ISchedulingRule getRule( final int kind, final Map<String, String> args ) {
    // prevent other project operations, but operations elsewhere in the workspace are fine
    return getProject();
  }

  static boolean hasCorrectExtension( final IPath path ) {
    if( path == null ) {
      return false;
    }
    String extension = path.getFileExtension();
    if( extension == null ) {
      return false;
    }
    return extension.equals( FileUtil.EXTENSION_HAPPY );
  }

  static void createMarker( final IResource resource, final ProcessorError e )
      throws CoreException {
    IMarker marker = resource
        .createMarker( HaskellCorePlugin.ID_HAPPY_MARKER );
    marker.setAttribute( IMarker.MESSAGE, e.getMessage() );
    marker.setAttribute( IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
    marker.setAttribute( IMarker.LINE_NUMBER, e.getLine() );
  }

}
