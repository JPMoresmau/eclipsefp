package net.sf.eclipsefp.haskell.ghccompiler.test.core;

import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.ghccompiler.core.Util;

/**
 * Test utilities
 * @author JP Moresmau
 *
 */
public class UtilTest extends TestCase {


  public UtilTest( final String name ) {
    super( name );
  }

  public void testIsTarget(){
    assertEquals(0,Util.compareTargets( "8.4.1", "8.4.1" ));

    assertEquals(1,Util.compareTargets( "8.4.1.2", "8.4.1" ));
    assertEquals(1,Util.compareTargets( "8.4.2", "8.4.1" ));
    assertEquals(1,Util.compareTargets( "8.6", "8.4.1" ));
    assertEquals(1,Util.compareTargets( "8.6.2", "8.4.1" ));
    assertEquals(1,Util.compareTargets( "8.4.1", "8.4" ));

    assertEquals(-1,Util.compareTargets( "8.4.1", "8.4.2" ));
    assertEquals(-1,Util.compareTargets( "8.4.1", "8.4.2.1" ));
    assertEquals(-1,Util.compareTargets( "8.4.1", "8.6" ));
    assertEquals(-1,Util.compareTargets( "8.4.1", "8.6.1" ));
  }
}
