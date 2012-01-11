/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.cabal;

import org.junit.Assert;
import org.junit.Test;

/**
 * Test operation on cabal package versions
 * @author JP Moresmau
 *
 */
public class CabalPackageVersionTest {

  @Test
  public void testCompare(){
    Assert.assertEquals( 0, CabalPackageVersion.compare( "1",  "1" ) );
    Assert.assertEquals( 0, CabalPackageVersion.compare( "1.0",  "1.0" ) );
    Assert.assertEquals( -1, CabalPackageVersion.compare( "1.0",  "1.1" ) );
    Assert.assertEquals( 1, CabalPackageVersion.compare( "1.1",  "1.0" ) );
    Assert.assertEquals( -1, CabalPackageVersion.compare( "1.1",  "1.1.1" ) );
    Assert.assertEquals( 1, CabalPackageVersion.compare( "1.1.1",  "1.1" ) );
    Assert.assertEquals( -1, CabalPackageVersion.compare( "1.0",  "2.0" ) );
    Assert.assertEquals( 1, CabalPackageVersion.compare( "2.0",  "1.0" ) );
  }
}
