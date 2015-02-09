/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.Choice;
import org.osgi.framework.Version;

/**
 * Possible versions of Cabal that may be set as minimal version required.
 * @author Alejandro Serrano
 *
 */
public class CabalVersionChoice extends Choice<String> {

  private static String[] values = new String[] { "1.2", "1.4", "1.6", "1.8", "1.10", "1.12" };

  static {
    Version v=CabalImplementationManager.getCabalLibraryVersion();
    if (v!=null){
      int min=v.getMinor();
      int max=v.getMajor();
      List<String> vals=new ArrayList<>();
      for (int a=1;a<=max;a++){
        for (int b=2;b<=min;b+=2){
          vals.add( a+"."+b );
        }
      }
      values=vals.toArray( new String[vals.size()]);
    }
  }

  @Override
  public String[] getValues() {
    return values;
  }

  @Override
  public boolean allowOther() {
    return true;
  }

  @Override
  public String fromCabalString( final String s ) {
    return s;
  }

  @Override
  public String toCabalString( final String o ) {
    return o;
  }

  @Override
  public String fromShownString( final String s ) {
    return s;
  }

  @Override
  public String toShownString( final String o ) {
    return o;
  }

}
