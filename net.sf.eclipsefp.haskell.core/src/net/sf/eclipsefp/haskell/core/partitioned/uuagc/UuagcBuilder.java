package net.sf.eclipsefp.haskell.core.partitioned.uuagc;

import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.partitioned.runner.ProcessorError;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
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
        && hasCorrectExtension( resource.getProjectRelativePath() ) && isInSourceFolder( ( IFile )resource ) );
  }

  static boolean isInSourceFolder( final IFile file ) {
    if( file == null || !file.isAccessible() ) {
      // throw new IllegalArgumentException();
      return false;
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
