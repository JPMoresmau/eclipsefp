/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageRef;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.backend.BackendManager;
import net.sf.eclipsefp.haskell.ui.internal.backend.CabalPackageHelper;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Content provider for the list of available dependencies.
 * @author Alejandro Serrano
 *
 */
public class DependenciesDialogContentProvider implements ITreeContentProvider {

  private HaskellPackage[] elements;

  public DependenciesDialogContentProvider( final List<String> alreadySelected,final String projectName ) {
    super();

    try {
       Set<String> names=new HashSet<>(alreadySelected);
      //BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.ALL,
       //   null );

      ArrayList<HaskellPackage> pkgs = new ArrayList<>();

      for( HaskellPackage pkg: BrowserPlugin.getSharedInstance().getPackages(Database.ALL) ) {
        if(! names.contains( pkg.getIdentifier().getName() ) ) {
          pkgs.add( pkg );
          names.add( pkg.getIdentifier().getName() );
        }

      }
      // if we don't have Hackage loaded, ask Cabal for all packages
      boolean hasHackage=BrowserPlugin.getSharedInstance().isHackageDatabaseLoaded();
      List<CabalPackageRef> cabalPkgs=hasHackage
            ?CabalPackageHelper.getInstance().getInstalled()
            :CabalPackageHelper.getInstance().getAll();

      /*
       * we may have installed packages not from hackage!
       */
      for (CabalPackageRef r:cabalPkgs){
        if (!names.contains( r.getName() )) {
          List<String> versions=new ArrayList<String>(hasHackage
              ?r.getInstalled()
              :r.getVersions());
          Collections.reverse(versions);
          HaskellPackage pkg=new HaskellPackage( "", new PackageIdentifier( r.getName(), versions.iterator().next() ) ) ;
          pkgs.add(pkg );
          names.add( r.getName() );
        }

      }


      for (HaskellPackage pkg:BackendManager.listProjectPackages()){
        // we can reference ourselves, or reference other projects if we're sandboxed
        if (BackendManager.getCabalImplDetails().isSandboxed() || pkg.getIdentifier().getName().equals( projectName ) ){
          if(! names.contains( pkg.getIdentifier().getName() ) ) {
            pkgs.add( pkg );
          }
        }
      }

      this.elements = pkgs.toArray( new HaskellPackage[ pkgs.size() ] );
    } catch( Throwable ex ) {
      HaskellUIPlugin.log( ex );
      this.elements = new HaskellPackage[ 0 ];
    }
  }

  @Override
  public void dispose() {
    // Do nothing
  }

  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
    // Do nothing
  }

  @Override
  public Object[] getElements( final Object inputElement ) {
    return elements;
  }

  @Override
  public Object[] getChildren( final Object parentElement ) {
    return new Object[ 0 ];
  }

  @Override
  public Object getParent( final Object element ) {
    // one level
    return null;
  }

  @Override
  public boolean hasChildren( final Object element ) {
    // one level
    return false;
  }

}
