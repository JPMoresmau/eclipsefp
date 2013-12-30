/**
 *
 */
package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IFile;


/**
 * Represents a folder in Cabal outline for Project Explorer
 * @author Alejandro Serrano
 *
 */
public class CabalFolder {
  IFile cabalFile;
  CabalFolderType type;

  public CabalFolder(final IFile cabalFile, final CabalFolderType type) {
    this.cabalFile = cabalFile;
    this.type = type;
  }

  public CabalFolderType getType() {
    return this.type;
  }

  public List<ProjectExplorerStanza> getStanzas() {
    try {
      PackageDescription descr = PackageDescriptionLoader.load( cabalFile );
      switch(this.type) {
        case EXECUTABLE:
          return getStanzas(descr.getExecutableStanzas());
        case TEST_SUITE:
          return getStanzas(descr.getTestSuiteStanzas());
        case BENCHMARK:
          return getStanzas(descr.getBenchmarkStanzas());
      }
      return new ArrayList<ProjectExplorerStanza>();
    } catch (Throwable e) {
      HaskellUIPlugin.log( e );
      return new ArrayList<ProjectExplorerStanza>();
    }
  }

  private List<ProjectExplorerStanza> getStanzas(final List<PackageDescriptionStanza> pds){
    List<ProjectExplorerStanza> ret=new ArrayList<ProjectExplorerStanza>(pds.size());
    for (PackageDescriptionStanza st:pds){
      ret.add( new ProjectExplorerStanza( cabalFile, st ) );
    }
    return ret;
  }
}
