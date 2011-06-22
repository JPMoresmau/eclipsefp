package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.ArrayList;
import java.util.Vector;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import org.apache.tools.ant.util.StringUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;


public class ModulesContentProvider implements ITreeContentProvider {

  public class ModulesVisitor implements IResourceVisitor {

    public ArrayList<String> elts;
    public Vector<String> possiblePrefixes;

    public ModulesVisitor( final ArrayList<String> whereAdd, final Vector<String> dirs ) {
      this.elts = whereAdd;
      this.possiblePrefixes = new Vector<String>();
      for( String dir: dirs ) {
        this.possiblePrefixes.add( dir.trim() + "/" );
      }
    }

    public boolean visit( final IResource resource ) {
      String path = resource.getProjectRelativePath().toString();
      for( String dir: possiblePrefixes ) {
        if( path.startsWith( dir )) {
          String filePath = path.substring( dir.length() );
          if (filePath.endsWith( ".hs" )) {
            String module = path.substring( 0, filePath.length() - 3 ).replace( '/', '.' );
            this.elts.add( module );
          } else if (filePath.endsWith( ".lhs" )) {
            String module = path.substring( 0, filePath.length() - 4 ).replace( '/', '.' );
            this.elts.add( module );
          }
        }
      }
      return true;
    }
  }

  public String[] elements;

  public ModulesContentProvider( final IProject project,
      final PackageDescription description,
      final PackageDescriptionStanza stanza ) throws CoreException {
    String sourceDirs = null;
    if (stanza.getProperties().containsKey( CabalSyntax.FIELD_HS_SOURCE_DIRS )) {
      sourceDirs = stanza.getProperties().get( CabalSyntax.FIELD_HS_SOURCE_DIRS );
    } else {
      sourceDirs = "";
    }

    ArrayList<String> modules = new ArrayList<String>();
    ModulesVisitor visitor = new ModulesVisitor( modules, StringUtils.split( sourceDirs, ',' ) );
    project.accept( visitor );

    PackageDescriptionStanza pkg = description.getPackageStanza();
    if (pkg != null) {
      if (pkg.getProperties().containsKey( CabalSyntax.FIELD_DATA_FILES )) {
        // There are data files, so Paths_package is also provided
        modules.add( "Paths_" + pkg.getProperties().get( CabalSyntax.FIELD_NAME ) );
      }
    }

    this.elements = modules.toArray(new String[modules.size()]);
  }

  public void dispose() {
    // Do nothing
  }

  public void inputChanged( final Viewer viewer, final Object oldInput,
      final Object newInput ) {
    // Do nothing
  }

  public Object[] getElements( final Object inputElement ) {
    return elements;
  }

  public Object[] getChildren( final Object parentElement ) {
    return new Object[ 0 ];
  }

  public Object getParent( final Object element ) {
    return null;
  }

  public boolean hasChildren( final Object element ) {
    return false;
  }

}
