package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcParameter;

/**
 * test parameter enum
 * @author JP Moresmau
 *
 */
public class GhcParameterTest extends TestCase {

  public GhcParameterTest( final String name ) {
    super( name );
  }

  public void testNameWithVersion(){
    assertEquals("-ffi",GhcParameter.LANG_FI.getName( "6.6.1" ));
    assertEquals("-XForeignFunctionInterface",GhcParameter.LANG_FI.getName( "6.8.1" ));
    assertEquals("-XForeignFunctionInterface",GhcParameter.LANG_FI.getName( "6.10.4" ));

    assertEquals("-fglasgow-exts",GhcParameter.LANG_GLASGOW_EXTS.getName( "6.6.1" ));
    assertEquals("-fglasgow-exts",GhcParameter.LANG_GLASGOW_EXTS.getName( "6.8.1" ));
    assertEquals("-fglasgow-exts",GhcParameter.LANG_GLASGOW_EXTS.getName( "6.10.4" ));

  }
}
