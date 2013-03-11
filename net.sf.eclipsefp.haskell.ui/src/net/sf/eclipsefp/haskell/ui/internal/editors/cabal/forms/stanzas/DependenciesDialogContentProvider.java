/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageHelper;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageRef;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
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
      Set<String> names=new HashSet<String>(alreadySelected);
      //BrowserPlugin.getSharedInstance().setCurrentDatabase( DatabaseType.ALL,
       //   null );
      ArrayList<HaskellPackage> pkgs = new ArrayList<HaskellPackage>();
      for( HaskellPackage pkg: BrowserPlugin.getSharedInstance().getPackages(Database.ALL) ) {
        if(! names.contains( pkg.getIdentifier().getName() ) ) {
          pkgs.add( pkg );
          names.add( pkg.getIdentifier().getName() );
        }
      }
      /**
       * we may have installed packages not from hackage!
       */
      for (CabalPackageRef r:CabalPackageHelper.getInstance().getInstalled()){
        if (!names.contains( r.getName() )) {
          pkgs.add( new HaskellPackage( "", new PackageIdentifier( r.getName(), r.getInstalled().iterator().next() ) ) );
          names.add( r.getName() );
        }
      }

      for (HaskellPackage pkg:ScionManager.listProjectPackages()){
        // we can reference ourselves, or reference other projects if we're sandboxed
        if (ScionManager.getCabalImplDetails().isSandboxed() || pkg.getIdentifier().getName().equals( projectName ) ){
          if(! names.contains( pkg.getIdentifier().getName() ) ) {
            pkgs.add( pkg );
          }
        }
      }


      this.elements = pkgs.toArray( new HaskellPackage[ pkgs.size() ] );
    } catch( Throwable ex ) {
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
