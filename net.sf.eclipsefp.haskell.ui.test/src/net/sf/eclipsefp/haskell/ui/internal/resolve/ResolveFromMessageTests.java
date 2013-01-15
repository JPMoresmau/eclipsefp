/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import java.util.List;
import org.junit.Test;


/**
 * Test resolution from GHCMessages
 * @author JP Moresmau
 *
 */
public class ResolveFromMessageTests {

  @Test
  public void testOneSuggestion(){
    String msg="Perhaps you meant `maybe' (imported from Prelude)";
    List<String> sugs=ReplaceTextResolution.getSuggestionsFromGHCMessage( msg );
    assertNotNull( sugs );
    assertEquals( 1, sugs.size() );
    assertEquals("maybe",sugs.get( 0 ));
  }

  @Test
  public void testTwoSuggestions(){
    String msg="Perhaps you meant one of these:\n      `unzip' (imported from Prelude), `unzip3' (imported from Prelude)";
    List<String> sugs=ReplaceTextResolution.getSuggestionsFromGHCMessage( msg );
    assertNotNull( sugs );
    assertEquals( 2, sugs.size() );
    assertEquals("unzip",sugs.get( 0 ));
    assertEquals("unzip3",sugs.get( 1 ));
  }

  @Test
  public void testQualifiedSuggestion(){
    String msg="Perhaps you meant List.foldl' (imported from Data.List)";
    List<String> sugs=ReplaceTextResolution.getSuggestionsFromGHCMessage( msg );
    assertNotNull( sugs );
    assertEquals( 1, sugs.size() );
    assertEquals("List.foldl'",sugs.get( 0 ));
  }


}
