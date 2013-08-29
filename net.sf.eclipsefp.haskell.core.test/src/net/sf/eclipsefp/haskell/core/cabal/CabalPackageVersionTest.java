/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.cabal;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 * Test operation on cabal package versions
 * @author JP Moresmau
 *
 */
public class CabalPackageVersionTest {

  @Test
  public void testCompare(){
    assertEquals( 0, CabalPackageVersion.compare( "1",  "1" ) );
    assertEquals( 0, CabalPackageVersion.compare( "1.0",  "1.0" ) );
    assertEquals( -1, CabalPackageVersion.compare( "1.0",  "1.1" ) );
    assertEquals( 1, CabalPackageVersion.compare( "1.1",  "1.0" ) );
    assertEquals( -1, CabalPackageVersion.compare( "1.1",  "1.1.1" ) );
    assertEquals( 1, CabalPackageVersion.compare( "1.1.1",  "1.1" ) );
    assertEquals( -1, CabalPackageVersion.compare( "1.0",  "2.0" ) );
    assertEquals( 1, CabalPackageVersion.compare( "2.0",  "1.0" ) );
    assertEquals( 1, CabalPackageVersion.compare( "0.2.16",  "0.2.12" ) );
    assertEquals( -1, CabalPackageVersion.compare( "0.2.12",  "0.2.16" ) );
  }

  @Test
  public void testRanges(){
    assertEquals(">=2.4 && <2.5",CabalPackageVersion.getMajorRange( "2.4" ));
    assertEquals(">=2.4 && <2.5",CabalPackageVersion.getMajorRange( "2.4.1" ));
    assertEquals(">=2.4 && <2.5",CabalPackageVersion.getMajorRange( "2.4.2" ));
    assertEquals(">=2.4 && <2.5",CabalPackageVersion.getMajorRange( "2.4.2.10" ));

    assertEquals(">=2.4 && <2.5",CabalPackageVersion.getMajorRangeFromMinor( "2.4" ));
    assertEquals(">=2.4.1 && <2.5",CabalPackageVersion.getMajorRangeFromMinor( "2.4.1" ));
    assertEquals(">=2.4.2 && <2.5",CabalPackageVersion.getMajorRangeFromMinor( "2.4.2" ));
    assertEquals(">=2.4.2 && <2.5",CabalPackageVersion.getMajorRangeFromMinor( "2.4.2.10" ));

    assertEquals(">=2.4 && <2.4.1",CabalPackageVersion.getMinorRange( "2.4" ));
    assertEquals(">=2.4.1 && <2.4.2",CabalPackageVersion.getMinorRange( "2.4.1" ));
    assertEquals(">=2.4.2 && <2.4.3",CabalPackageVersion.getMinorRange( "2.4.2" ));
    assertEquals(">=2.4.2 && <2.4.3",CabalPackageVersion.getMinorRange( "2.4.2.1O" ));

  }
}
