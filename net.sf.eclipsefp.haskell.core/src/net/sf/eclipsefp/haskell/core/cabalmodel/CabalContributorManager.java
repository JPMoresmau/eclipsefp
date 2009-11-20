package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.util.ArrayList;
import java.util.Collection;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

/**
 *
 * @author jean-philippem
 *
 */
public class CabalContributorManager {
  private static Collection<ICabalContributor> contribs=null;

    public static Collection<ICabalContributor> getContributors(){
      if (contribs==null){
        IConfigurationElement[] elts=HaskellCorePlugin.getDefault().getExtensions( HaskellCorePlugin.ID_EXT_POINT_CABAL_CONTRIBUTORS );
        contribs=new ArrayList<ICabalContributor>(elts.length);
        for (IConfigurationElement elem:elts){
          try {
            Object o = elem.createExecutableExtension(HaskellCorePlugin.ATT_CLASS);
            contribs.add( (ICabalContributor )o);
          } catch (CoreException cex) {
            HaskellCorePlugin.log( cex );
          }
        }
      }
      return contribs;
    }
}
