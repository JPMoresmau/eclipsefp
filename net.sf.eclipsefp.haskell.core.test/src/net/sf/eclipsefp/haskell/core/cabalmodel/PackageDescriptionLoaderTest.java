/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.util.Arrays;
import java.util.List;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.junit.Assert;
import org.junit.Test;


/**
 * Test methods of package description loader
 * @author JP Moresmau
 *
 */
public class PackageDescriptionLoaderTest {

  @Test
  public void parseList(){
    List<String> ls=PackageDescriptionLoader.parseList( "toto,titi, tutu" );
    Assert.assertEquals( Arrays.asList( "toto","titi","tutu"),ls );
    ls=PackageDescriptionLoader.parseList( "toto > 0.1,titi > 0.1 && < 1.0,"+PlatformUtil.NL+" tutu" );
    Assert.assertEquals( Arrays.asList( "toto > 0.1","titi > 0.1 && < 1.0","tutu"),ls );
  }
}
