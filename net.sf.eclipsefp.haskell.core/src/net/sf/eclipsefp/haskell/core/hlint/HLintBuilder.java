package net.sf.eclipsefp.haskell.core.hlint;

import java.util.Map;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.hlint.Severity;
import net.sf.eclipsefp.haskell.hlint.Suggestion;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * HLint builder: calls HLint, parses the results and shows
 * them as file markers in the editor.
 *
 * @author Alejandro Serrano
 */
public class HLintBuilder extends IncrementalProjectBuilder {

  public static String BUILDER_ID = HLintBuilder.class.getName();

  public HLintBuilder() {
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
      ResourcesPlugin.getWorkspace().run( new IWorkspaceRunnable() {
        public void run( final IProgressMonitor monitor ) {
          try {
            delta.accept( new DeltaVisitor() );
          } catch (CoreException e) {
            // Do nothing
          }
        }
      }, monitor );
    } else if( kind == CLEAN_BUILD ) {
      ResourcesPlugin.getWorkspace().run( new IWorkspaceRunnable() {
        public void run( final IProgressMonitor monitor ) throws CoreException {
          clean( monitor );
        }
      }, monitor );
    } else if( kind == FULL_BUILD ) {
      ResourcesPlugin.getWorkspace().run( new IWorkspaceRunnable() {
        public void run( final IProgressMonitor monitor ) throws CoreException {
          clean( monitor );
          try {
            getProject().accept( new FullBuildVisitor() );
          } catch (CoreException e) {
            // Do nothing
          }
        }
      }, monitor );
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
        && hasCorrectExtension( resource.getProjectRelativePath() )
        && ResourceUtil.isInSourceFolder( ( IFile )resource )
        && !resource.isDerived() );
  }


  static boolean hasCorrectExtension( final IPath path ) {
    if( path == null ) {
      return false;
    }
    String extension = path.getFileExtension();
    if( extension == null ) {
      return false;
    }
    return extension.equals( FileUtil.EXTENSION_HS );
  }

  static void createMarker( final IResource resource, final Suggestion s )
      throws CoreException {
    if( s.getSeverity() != Severity.IGNORE ) {
      IMarker marker = resource
          .createMarker( HaskellCorePlugin.ID_HLINT_MARKER );
      marker.setAttribute( IMarker.MESSAGE, s.getMessage() );
      marker.setAttribute( IMarker.SEVERITY,
          s.getSeverity() == Severity.ERROR ? IMarker.SEVERITY_WARNING
              : IMarker.SEVERITY_INFO );
      // marker.setAttribute( IMarker.CHAR_START, 0 );
      // marker.setAttribute( IMarker.CHAR_END, 1 );
      marker.setAttribute( IMarker.LINE_NUMBER, s.getLocation().getLine() );
    }
  }

}
