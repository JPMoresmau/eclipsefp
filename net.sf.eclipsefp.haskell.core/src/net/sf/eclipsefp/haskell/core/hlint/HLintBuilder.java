package net.sf.eclipsefp.haskell.core.hlint;

import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
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

public class HLintBuilder extends IncrementalProjectBuilder {

  public static String BUILDER_ID = HLintBuilder.class.getName();

  public HLintBuilder() {
    // Do nothing
  }

  @Override
  protected IProject[] build( final int kind, final Map<String, String> args,
      final IProgressMonitor monitor ) throws CoreException {
    if( kind == INCREMENTAL_BUILD || kind == AUTO_BUILD ) {
      // Get delta
      final IResourceDelta delta = getDelta( getProject() );
      if( delta == null ) {
        return null;
      }
      ResourcesPlugin.getWorkspace().run( new IWorkspaceRunnable() {
        @Override
        public void run( final IProgressMonitor monitor ) throws CoreException {
          delta.accept( new DeltaVisitor() );
        }
      }, monitor );
    } else if( kind == CLEAN_BUILD ) {
      ResourcesPlugin.getWorkspace().run( new IWorkspaceRunnable() {
        @Override
        public void run( final IProgressMonitor monitor ) throws CoreException {
          clean( monitor );
        }
      }, monitor );
    } else if( kind == FULL_BUILD ) {
      ResourcesPlugin.getWorkspace().run( new IWorkspaceRunnable() {
        @Override
        public void run( final IProgressMonitor monitor ) throws CoreException {
          clean( monitor );
          getProject().accept( new FullBuildVisitor() );
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
        && hasCorrectExtension( resource.getProjectRelativePath() ) && isInSourceFolder( ( IFile )resource ) );
  }

  static boolean isInSourceFolder( final IFile file ) {
    if( file == null || !file.isAccessible() ) {
      throw new IllegalArgumentException();
    }
    boolean result = false;
    IHaskellProject hsProject = HaskellProjectManager.get( file.getProject() );
    Set<IPath> sourcePaths = hsProject.getSourcePaths();
    for( IPath sourcePath: sourcePaths ) {
      IPath src = file.getProject().getFullPath().append( sourcePath );
      result |= src.isPrefixOf( file.getFullPath() );
    }
    return result;
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
