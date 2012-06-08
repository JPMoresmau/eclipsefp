/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import static org.junit.Assert.assertEquals;
import net.sf.eclipsefp.haskell.buildwrapper.types.GhcMessages;
import org.junit.Test;


/**
 * Test missing type warning, since several different type syntaxes may cause the wrong type to be automatically added
 * @author JP Moresmau
 *
 */
public class MissingTypeWarningTest {

  @Test
  public void testWithPackage(){
     String full="Top-level binding with no type signature:\n               commonHome :: forall s a a1.\n                             (ToWidget s App a1, blaze-markup-0.5.1.0:Text.Blaze.ToMarkup a) =>\n                             a1 -> a -> Maybe Search -> GHandler s App RepHtml";
     String type="commonHome :: forall s a a1.\n                             (ToWidget s App a1, Text.Blaze.ToMarkup a) =>\n                             a1 -> a -> Maybe Search -> GHandler s App RepHtml";
     assertEquals(type,MissingTypeWarningResolution.extractTypeFromMessage( GhcMessages.WARNING_NOTYPE_TOPLEVEL_CONTAINS, full ));
  }

  @Test
  public void testWith2Packages(){
     String full="Top-level binding with no type signature:\n               commonHome :: forall s a a1.\n                             (blaze-markup-0.5.1.0:Text.Blaze.ToMarkup a1, blaze-markup-0.5.1.0:Text.Blaze.ToMarkup a) =>\n                             a1 -> a -> Maybe Search -> GHandler s App RepHtml";
     String type="commonHome :: forall s a a1.\n                             (Text.Blaze.ToMarkup a1, Text.Blaze.ToMarkup a) =>\n                             a1 -> a -> Maybe Search -> GHandler s App RepHtml";
     assertEquals(type,MissingTypeWarningResolution.extractTypeFromMessage( GhcMessages.WARNING_NOTYPE_TOPLEVEL_CONTAINS, full ));
  }

  @Test
  public void testWithKind(){
    String full="Top-level binding with no type signature:\n               f :: forall (x :: * -> *) y t. (t -> x y) -> t -> D x y";
    String type="f :: forall (x :: * -> *) y t. (t -> x y) -> t -> D x y";
    assertEquals(type,MissingTypeWarningResolution.extractTypeFromMessage( GhcMessages.WARNING_NOTYPE_TOPLEVEL_CONTAINS, full ));
  }

  @Test
  public void testForAll(){
    String full="Top-level binding with no type signature:\n           fun :: forall t. t -> [Char] -> [Char]";
    String type="fun :: forall t. t -> [Char] -> [Char]";
    assertEquals(type,MissingTypeWarningResolution.extractTypeFromMessage( GhcMessages.WARNING_NOTYPE_TOPLEVEL_CONTAINS, full ));
  }

}
