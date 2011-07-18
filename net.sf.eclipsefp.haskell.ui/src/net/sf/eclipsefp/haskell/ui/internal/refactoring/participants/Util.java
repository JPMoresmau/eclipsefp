package net.sf.eclipsefp.haskell.ui.internal.refactoring.participants;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.util.FileUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;


public class Util {

  public static Set<IPath> getPaths( final IProject project ) {
    return HaskellProjectManager.get( project ).getSourcePaths();
  }

  public static String getModuleName( final IResource resource ) {
    return getModuleName( resource.getProject(),
        resource.getProjectRelativePath() );
  }

  public static List<IResource> getHaskellFiles( final IProject project,
      final IFile file ) {
    final ArrayList<IResource> resources = new ArrayList<IResource>();
    try {
      project.accept( new IResourceVisitor() {
        public boolean visit( final IResource resource ) {
          if( resource instanceof IFile && !( file.equals( resource ) )
              && FileUtil.hasHaskellExtension( resource ) ) {
            resources.add( resource );
          }
          return true;
        }
      } );
    } catch( Exception e ) {
      // Do nothing
    }
    return resources;
  }

  public static String getModuleName( final IProject project,
      final IPath projectRelativePath ) {
    String path = projectRelativePath.toPortableString();
    for( IPath prefix: getPaths( project ) ) {
      String prefixPath = prefix.toPortableString();
      if( path.startsWith( prefixPath ) ) {
        String filePath = path.substring( prefixPath.length() + 1 );
        if( filePath.endsWith( ".hs" ) ) {
          return filePath.substring( 0, filePath.length() - 3 ).replace( '/', '.' );
        } else if( filePath.endsWith( ".lhs" ) ) {
          return filePath.substring( 0, filePath.length() - 4 ).replace( '/', '.' );
        }
      }
    }

    return null;
  }

  public static int getModuleNameOffset(final IResource resource) {
    TextFileDocumentProvider provider = new TextFileDocumentProvider();

    try {
      provider.connect( resource );
    } catch (CoreException e) {
      return -1;
    }

    int offset = -1;
    try {
      IDocument doc = provider.getDocument( resource );
      int numberOfLines = doc.getNumberOfLines();
      for (int i = 0; i < numberOfLines; i++) {
        IRegion region = doc.getLineInformation( i );
        String text = doc.get( region.getOffset(), region.getLength() );
        String[] words = text.split( " " );
        if (words[0].equals( "module" )) {
          // We are in a module declaration
          String moduleName = words[1];
          if (moduleName.length() == 0) {
            moduleName = words[2];
          }
          int moduleNamePos = text.indexOf( moduleName );
          offset = region.getOffset() + moduleNamePos;
          break;
        }
      }
    } catch (BadLocationException e) {
      // This should not happen
    } finally {
      provider.disconnect( resource );
    }

    return offset;
  }
}
