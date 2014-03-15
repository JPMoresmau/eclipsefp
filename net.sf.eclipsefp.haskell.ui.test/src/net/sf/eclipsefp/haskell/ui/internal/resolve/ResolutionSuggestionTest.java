/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Locale;
import net.sf.eclipsefp.haskell.buildwrapper.types.GhcMessages;
import org.junit.Test;

/**
 * test not in scope message parsing
 *
 * @author JP Moresmau
 *
 */
public class ResolutionSuggestionTest {

   @Test
   public void testFoldlQuoteMultiple(){
     String msg="Not in scope: foldl'\n"+
         "Perhaps you meant one of these:\n"+
         "  DM.foldl' (imported from Data.Map),\n"+
         "  `foldl' (imported from Prelude),\n"+
         "  `DM.foldl' (imported from Data.Map)\n";
     String msgL=msg.toLowerCase(Locale.ENGLISH);
     int ix=msgL.indexOf( GhcMessages.NOT_IN_SCOPE_START);
     assertTrue(ix>-1);
     ResolutionSuggestion s=new ResolutionSuggestion( msg, ix,msgL );
     assertEquals(3,s.getSuggestions().size());
     assertEquals("foldl'",s.getOutOfScope());
     assertEquals("foldl'",s.getOutOfScopeName());
     assertNull(s.getOutOfScopeQualifier());
   }

   @Test
   public void testFoldlQuoteNone(){
     String msg="Not in scope: foldl'";
     String msgL=msg.toLowerCase(Locale.ENGLISH);
     int ix=msgL.indexOf( GhcMessages.NOT_IN_SCOPE_START);
     assertTrue(ix>-1);
     ResolutionSuggestion s=new ResolutionSuggestion( msg, ix,msgL );
     assertNull(s.getSuggestions());
     assertEquals("foldl'",s.getOutOfScope());
     assertEquals("foldl'",s.getOutOfScopeName());
     assertNull(s.getOutOfScopeQualifier());
   }
   @Test
   public void testFoldMMultiple(){
     String msg="Not in scope: `foldM'\n"+
         "Perhaps you meant one of these:\n"+
         "  `foldr' (imported from Prelude),\n"+
         "  `DM.foldr' (imported from Data.Map),\n"+
         "  foldl' (imported from Prelude)\n";
     String msgL=msg.toLowerCase(Locale.ENGLISH);
     int ix=msgL.indexOf( GhcMessages.NOT_IN_SCOPE_START);
     assertTrue(ix>-1);
     ResolutionSuggestion s=new ResolutionSuggestion( msg, ix,msgL );
     assertEquals(3,s.getSuggestions().size());
     assertEquals("foldM",s.getOutOfScope());
     assertEquals("foldM",s.getOutOfScopeName());
     assertNull(s.getOutOfScopeQualifier());
   }

   @Test
   public void testNone(){
     String msg="Not in scope: `assertEqual'";
     String msgL=msg.toLowerCase(Locale.ENGLISH);
     int ix=msgL.indexOf( GhcMessages.NOT_IN_SCOPE_START);
     assertTrue(ix>-1);
     ResolutionSuggestion s=new ResolutionSuggestion( msg, ix,msgL );
     assertNull(s.getSuggestions());
     assertEquals("assertEqual",s.getOutOfScope());
     assertEquals("assertEqual",s.getOutOfScopeName());
     assertNull(s.getOutOfScopeQualifier());
   }

   @Test
   public void testType(){
     String msg="Not in scope: type constructor or class `Array'";
     String msgL=msg.toLowerCase(Locale.ENGLISH);
     int ix=msgL.indexOf( GhcMessages.NOT_IN_SCOPE_START);
     assertTrue(ix>-1);
     ResolutionSuggestion s=new ResolutionSuggestion( msg, ix,msgL );
     assertNull(s.getSuggestions());
     assertEquals("Array",s.getOutOfScope());
     assertEquals("Array",s.getOutOfScopeName());
     assertNull(s.getOutOfScopeQualifier());

   }
}
