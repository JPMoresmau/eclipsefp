/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.refactoring.participants;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.types.SearchResultLocation;
import net.sf.eclipsefp.haskell.buildwrapper.types.UsageResults;
import net.sf.eclipsefp.haskell.buildwrapper.usage.UsageAPI;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;

/**
 * Utilities and shared changes creation for participants.
 * @author Alejandro Serrano
 * @author JP Moresmau
 */
public class Util {

  public static Set<IPath> getPaths( final IProject project ) {
    HashSet<IPath> paths = new HashSet<>();

    try {
      IFile cabalF = BuildWrapperPlugin.getCabalFile( project );
      PackageDescription pd = PackageDescriptionLoader.load( cabalF );
      List<PackageDescriptionStanza> lpds = pd.getStanzas();
      for( PackageDescriptionStanza pds: lpds ) {
        String propList = pds.getProperties().get(
            CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName().toLowerCase() );
        propList = propList == null ? "" : propList;
        List<String> props = PackageDescriptionLoader.parseList( propList );

        for (String prop : props) {
          IPath prefixPath = Path.fromPortableString( prop );
          paths.add( prefixPath );
        }
      }
    } catch (Exception e) {
      return new HashSet<>();
    }

    return paths;
  }

//  public static Set<IPath> getStanzaPaths( final IProject project,
//      final IFile file ) {
//    HashSet<IPath> paths = new HashSet<IPath>();
//
//    try {
//      IFile cabalF = BuildWrapperPlugin.getCabalFile( project );
//      PackageDescription pd = PackageDescriptionLoader.load( cabalF );
//      List<PackageDescriptionStanza> lpds = pd.getStanzas();
//      for( PackageDescriptionStanza pds: lpds ) {
//        String propList = pds.getProperties().get(
//            CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName().toLowerCase() );
//        propList = propList == null ? "" : propList;
//        List<String> props = PackageDescriptionLoader.parseList( propList );
//
//        // If the file is in one of these directories, we should change it
//        boolean shouldBeAdded = false;
//        for (String prop : props) {
//          IPath prefixPath = Path.fromPortableString( prop );
//          if (prefixPath.isPrefixOf( file.getProjectRelativePath() )) {
//            shouldBeAdded = true;
//          }
//        }
//
//        // If should be changed, add every directory in the stanza
//        if (shouldBeAdded) {
//          for (String prop : props) {
//            IPath prefixPath = Path.fromPortableString( prop );
//            paths.add( prefixPath );
//          }
//        }
//      }
//    } catch (Exception e) {
//      return new HashSet<IPath>();
//    }
//
//    return paths;
//  }
//
//  public static String getModuleName( final IResource resource ) {
//    return getModuleName( resource.getProject(),
//        resource.getProjectRelativePath() );
//  }

//  public static List<IResource> getHaskellFiles( final IProject project,
//      final IFile file ) {
//    final ArrayList<IResource> resources = new ArrayList<IResource>();
//    final Collection<String> srcs=ResourceUtil.getSourceFolders( new IFile[]{file} );
//
//    //final Set<IPath> allowedPaths = getStanzaPaths( project, file );
//    try {
//      project.accept( new IResourceVisitor() {
//
//        @Override
//        public boolean visit( final IResource resource ) {
//          if( resource instanceof IFile && !( file.equals( resource ) )
//              && FileUtil.hasHaskellExtension( resource )) {
//            // Check if we are in one of the allowed paths
//            for (String allowedPath : srcs) {
//              if (allowedPath.isPrefixOf( resource.getProjectRelativePath() )) {
//                resources.add( resource );
//                break;
//              }
//            }
//          }
//          return true;
//        }
//      } );
//    } catch( Exception e ) {
//      // Do nothing
//    }
//    return resources;
//  }

//  public static String getModuleName( final IProject project,
//      final IPath projectRelativePath ) {
//    String path = projectRelativePath.toPortableString();
//    for( IPath prefix: getPaths( project ) ) {
//      String prefixPath = prefix.toPortableString();
//      if( path.startsWith( prefixPath ) ) {
//        String filePath = path.substring( prefixPath.length() + 1 );
//        if( filePath.endsWith( ".hs" ) ) {
//          return filePath.substring( 0, filePath.length() - 3 ).replace( '/',
//              '.' );
//        } else if( filePath.endsWith( ".lhs" ) ) {
//          return filePath.substring( 0, filePath.length() - 4 ).replace( '/',
//              '.' );
//        }
//      }
//    }
//
//    return null;
//  }

  public static int getModuleNameOffset( final IResource resource,final String module ) {
    TextFileDocumentProvider provider = new TextFileDocumentProvider();

    try {
      provider.connect( resource );
    } catch( CoreException e ) {
      return -1;
    }

    int offset = -1;
    try {
      IDocument doc = provider.getDocument( resource );
      /*int numberOfLines = doc.getNumberOfLines();
      for( int i = 0; i < numberOfLines; i++ ) {
        IRegion region = doc.getLineInformation( i );
        String text = doc.get( region.getOffset(), region.getLength() );
        String[] words = text.split( " " );
        if( words[ 0 ].equals( "module" ) ) {
          // We are in a module declaration
          String moduleName = words[ 1 ];
          if( moduleName.length() == 0 ) {
            moduleName = words[ 2 ];
          }
          int moduleNamePos = text.indexOf( moduleName );
          offset = region.getOffset() + moduleNamePos;
          break;
        }
      }*/
      UsageAPI api=BuildWrapperPlugin.getDefault().getUsageAPI();
      if (api!=null){
        UsageResults ur=api.getModuleDefinitions( null, module, resource.getProject(), true );
        Map<IFile,Map<String,Collection<SearchResultLocation>>> m1=ur.getUsageInProject( resource.getProject() );
        if (m1!=null){
          Map<String,Collection<SearchResultLocation>> m=m1.get( resource );
          if (m!=null){
            Collection<SearchResultLocation> srls=m.get( "module "+module );
            if (srls!=null){
              for (SearchResultLocation srl:srls){
                offset=srl.getStartOffset( doc );
                return offset;
              }
            }
          }
        }
      }
    } catch( BadLocationException e ) {
      // This should not happen
    } finally {
      provider.disconnect( resource );
    }

    return offset;
  }

  public static List<Integer> getImportModuleOffsets( final IResource resource,
      final String moduleName ) {
    TextFileDocumentProvider provider = new TextFileDocumentProvider();
    ArrayList<Integer> offsets = new ArrayList<>();

    try {
      provider.connect( resource );
    } catch( CoreException e ) {
      return offsets;
    }

    try {
      IDocument doc = provider.getDocument( resource );
      int numberOfLines = doc.getNumberOfLines();
      for( int i = 0; i < numberOfLines; i++ ) {
        IRegion region = doc.getLineInformation( i );
        String text = doc.get( region.getOffset(), region.getLength() );
        String[] words = text.split( " " );

        for( int j = 0; j < words.length; j++ ) {
          if( words[ j ].equals( "module" ) ) {
            try {
              String mname = words[ j + 1 ];
              if( mname.length() == 0 ) {
                mname = words[ j + 2 ];
              }
              if( mname.equals( moduleName ) ) {
                int modulePos = text.indexOf( mname );
                offsets.add( region.getOffset() + modulePos );
              }
            } catch( Exception e ) {
              // We are out of bounds
            }
            break;
          } else if( words[ j ].equals( "import" ) ) {
            try {
              String mname = words[ j + 1 ];
              if( mname.length() == 0 ) {
                mname = words[ j + 2 ];
              }
              if( mname.equals( "qualified" ) ) {
                mname = words[ j + 3 ];
                if( mname.length() == 0 ) {
                  mname = words[ j + 4 ];
                }
              }
              if( mname.equals( moduleName ) ) {
                int modulePos = text.indexOf( mname );
                offsets.add( region.getOffset() + modulePos );
              }
            } catch( Exception e ) {
              // We are out of bounds
            }
            break;
          }
        }
      }
    } catch( BadLocationException e ) {
      // This should not happen
    } finally {
      provider.disconnect( resource );
    }

    return offsets;
  }

  public static String newCabalFile( final IProject project,
      final IFile oldFile, final String newModuleName ) {
    TextFileDocumentProvider provider = new TextFileDocumentProvider();
    String oldModuleName = ResourceUtil.getModuleName( oldFile );

    IFile cabalF = BuildWrapperPlugin.getCabalFile( project );
    PackageDescription pd = null;
    try {
      pd = PackageDescriptionLoader.load( cabalF );
      provider.connect( cabalF );
    } catch( Exception e ) {
      return null;
    }

    IDocument doc = provider.getDocument( cabalF );
    try {
      //List<PackageDescriptionStanza> lpds = pd.getStanzas();
      Set<PackageDescriptionStanza> lpds =ResourceUtil.getApplicableStanzas( new IFile[]{oldFile} );
      for( PackageDescriptionStanza pds: lpds ) {

        // Should we change this stanza?
//        String srcList = pds.getProperties().get(
//            CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName().toLowerCase() );
//        srcList = srcList == null ? "" : srcList;
//        List<String> srcs = PackageDescriptionLoader.parseList( srcList );
//        final Set<IPath> allowedPaths = getStanzaPaths( project, oldFile );
//        boolean shouldBeChanged = false;
//        for (String src : srcs) {
//          IPath srcPath = Path.fromPortableString( src );
//          if (allowedPaths.contains( srcPath )) {
//            shouldBeChanged = true;
//            break;
//          }
//        }
//        if (!shouldBeChanged) {
//          continue; // We should not
//        }

        // Modules sections
        CabalSyntax[] elements = new CabalSyntax[] {
            CabalSyntax.FIELD_EXPOSED_MODULES, CabalSyntax.FIELD_OTHER_MODULES };
        for( CabalSyntax element: elements ) {
          pds = pd.getSameStanza( pds );
          String propList = pds.getProperties().get(
              element.getCabalName().toLowerCase() );
          propList = propList == null ? "" : propList;
          List<String> props = PackageDescriptionLoader.parseList( propList );
          if( props.indexOf( oldModuleName ) != -1 ) {
            RealValuePosition rvp = pds.removeFromPropertyList( element,
                oldModuleName );
            rvp.updateDocument( doc );
            pd = PackageDescriptionLoader.load( doc.get() );
            pds = pd.getSameStanza( pds );

            rvp = pds.addToPropertyList( element, newModuleName );
            rvp.updateDocument( doc );
            pd = PackageDescriptionLoader.load( doc.get() );
            pds = pd.getSameStanza( pds );
          }
        }

        // Main-is section
        String oldMainName = oldModuleName.replace( '.', '/' ) + ".hs";
        String newMainName = newModuleName.replace( '.', '/' ) + ".hs";
        pds = pd.getSameStanza( pds );
        String mainProp = pds.getProperties().get(
            CabalSyntax.FIELD_MAIN_IS.getCabalName().toLowerCase() );
        mainProp = mainProp == null ? "" : mainProp;
        if( oldMainName.equals( mainProp ) ) {
          RealValuePosition rvp = pds.update( CabalSyntax.FIELD_MAIN_IS,
              newMainName );
          rvp.updateDocument( doc );
          pd = PackageDescriptionLoader.load( doc.get() );
          pds = pd.getSameStanza( pds );
        }

      }
    } catch( Exception e ) {
      // Do nothing
    } finally {
      provider.disconnect( cabalF );
    }

    return doc.get();
  }

  public static String newRemoveModuleCabalFile( final IProject project,
      final IFile oldFile ) {
    TextFileDocumentProvider provider = new TextFileDocumentProvider();
    String oldModuleName = ResourceUtil.getModuleName( oldFile );

    IFile cabalF = BuildWrapperPlugin.getCabalFile( project );
    PackageDescription pd = null;
    try {
      pd = PackageDescriptionLoader.load( cabalF );
      provider.connect( cabalF );
    } catch( Exception e ) {
      return null;
    }

    IDocument doc = provider.getDocument( cabalF );
    try {
     // List<PackageDescriptionStanza> lpds = pd.getStanzas();
      Set<PackageDescriptionStanza> lpds =ResourceUtil.getApplicableStanzas( new IFile[]{oldFile} );
      for( PackageDescriptionStanza pds: lpds ) {

//        // Should we change this stanza?
//        String srcList = pds.getProperties().get(
//            CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName().toLowerCase() );
//        srcList = srcList == null ? "" : srcList;
//        List<String> srcs = PackageDescriptionLoader.parseList( srcList );
//        final Set<IPath> allowedPaths = getStanzaPaths( project, oldFile );
//        boolean shouldBeChanged = false;
//        for (String src : srcs) {
//          IPath srcPath = Path.fromPortableString( src );
//          if (allowedPaths.contains( srcPath )) {
//            shouldBeChanged = true;
//            break;
//          }
//        }
//        if (!shouldBeChanged) {
//          continue; // We should not
//        }

        // Modules sections
        CabalSyntax[] elements = new CabalSyntax[] {
            CabalSyntax.FIELD_EXPOSED_MODULES, CabalSyntax.FIELD_OTHER_MODULES };
        for( CabalSyntax element: elements ) {
          pds = pd.getSameStanza( pds );
          String propList = pds.getProperties().get(
              element.getCabalName().toLowerCase() );
          propList = propList == null ? "" : propList;
          List<String> props = PackageDescriptionLoader.parseList( propList );
          if( props.indexOf( oldModuleName ) != -1 ) {
            RealValuePosition rvp = pds.removeFromPropertyList( element,
                oldModuleName );
            rvp.updateDocument( doc );
            pd = PackageDescriptionLoader.load( doc.get() );
            pds = pd.getSameStanza( pds );
          }
        }

        // Main-is section
        String oldMainName = oldModuleName.replace( '.', '/' ) + ".hs";
        pds = pd.getSameStanza( pds );
        String mainProp = pds.getProperties().get(
            CabalSyntax.FIELD_MAIN_IS.getCabalName().toLowerCase() );
        mainProp = mainProp == null ? "" : mainProp;
        if( oldMainName.equals( mainProp ) ) {
          RealValuePosition rvp = pds.update( CabalSyntax.FIELD_MAIN_IS, "" );
          rvp.updateDocument( doc );
          pd = PackageDescriptionLoader.load( doc.get() );
          pds = pd.getSameStanza( pds );
        }

      }
    } catch( Exception e ) {
      // Do nothing
    } finally {
      provider.disconnect( cabalF );
    }

    return doc.get();
  }

  public static String newSourceFolderCabalFile(final IProject project,
      final IPath oldPath, final IPath newPath) {
    TextFileDocumentProvider provider = new TextFileDocumentProvider();
    IFile cabalF = BuildWrapperPlugin.getCabalFile( project );
    PackageDescription pd = null;
    try {
      pd = PackageDescriptionLoader.load( cabalF );
      provider.connect( cabalF );
    } catch( Exception e ) {
      return null;
    }

    IDocument doc = provider.getDocument( cabalF );
    try {
      List<PackageDescriptionStanza> lpds = pd.getStanzas();
      for( PackageDescriptionStanza pds: lpds ) {

        // Should we change this stanza?
        String srcList = pds.getProperties().get(
            CabalSyntax.FIELD_HS_SOURCE_DIRS.getCabalName().toLowerCase() );
        srcList = srcList == null ? "" : srcList;
        List<String> srcs = PackageDescriptionLoader.parseList( srcList );
        for (String src : srcs) {
          IPath srcPath = Path.fromPortableString( src );
          if (srcPath.equals( oldPath )) {
            RealValuePosition rvp = pds.removeFromPropertyList(
                CabalSyntax.FIELD_HS_SOURCE_DIRS,
                oldPath.toPortableString() );
            rvp.updateDocument( doc );
            pd = PackageDescriptionLoader.load( doc.get() );
            pds = pd.getSameStanza( pds );

            rvp = pds.addToPropertyList(
                CabalSyntax.FIELD_HS_SOURCE_DIRS,
                newPath.toPortableString() );
            rvp.updateDocument( doc );
            pd = PackageDescriptionLoader.load( doc.get() );
            pds = pd.getSameStanza( pds );

            break;
          }
        }
      }
    } catch( Exception e ) {
      // Do nothing
    } finally {
      provider.disconnect( cabalF );
    }

    return doc.get();
  }
}
