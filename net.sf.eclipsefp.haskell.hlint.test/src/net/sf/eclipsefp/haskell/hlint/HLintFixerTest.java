/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.hlint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import net.sf.eclipsefp.haskell.hlint.HLintFixer.HLintFix;

import org.junit.Test;

/**
 * Test automatic hlint fixing
 * @author JP Moresmau
 *
 */
public class HLintFixerTest {

	
	@Test
	public void testListComprehension(){
		Suggestion sug=new Suggestion();
		sug.setPre(new CodeModificationText("  if isJust (PD.library pd) then\n    [CCLibrary\n       (PD.buildable $ PD.libBuildInfo $ fromJust (PD.library pd))]\n    else []"));
		sug.setPost(new CodeModificationText("[CCLibrary\n     (PD.buildable $ PD.libBuildInfo $ fromJust (PD.library pd)) \n   | isJust (PD.library pd)]"));
		sug.setLocation(new SourceLocation("src/Language/Haskell/BuildWrapper/Cabal.hs", 560, 8));
		sug.setMessage("Warning: Use list comprehension");
		
		String doc="      (if isJust (PD.library pd) then [CCLibrary (PD.buildable $ PD.libBuildInfo $ fromJust (PD.library pd))] else []) ++";
		HLintFix fix=HLintFixer.fix(doc, 7, sug);
		assertNotNull(fix);
		assertEquals(110,fix.getLength());
		String newDoc=doc.substring(0,7)+fix.getValue()+doc.substring(7+fix.getLength());
		assertEquals("      ([CCLibrary\n     (PD.buildable $ PD.libBuildInfo $ fromJust (PD.library pd)) \n   | isJust (PD.library pd)]) ++",newDoc);
	}
	
	@Test
	public void testRedundantBracket(){
		Suggestion sug=new Suggestion();
		sug.setPre(new CodeModificationText("  (map (\\ (a, b) -> (a, [b])) cpkgs) ++\n    (map (\\ (a, _) -> (a, [])) pkgs)"));
		sug.setPost(new CodeModificationText("  (map (\\ (a, b) -> (a, [b])) cpkgs) ++\n    map (\\ (a, _) -> (a, [])) pkgs"));
		sug.setLocation(new SourceLocation("src/Language/Haskell/BuildWrapper/Cabal.hs", 537, 48));
		sug.setMessage("Warning: Redundant bracket");
		
		String doc="        in DM.assocs $ DM.fromListWith (++) $ ((map (\\(a,b)->(a,[b])) cpkgs) ++ (map (\\(a,_)->(a,[])) pkgs))";
		HLintFix fix=HLintFixer.fix(doc, 47, sug);
		assertNotNull(fix);
		assertEquals(60,fix.getLength());
		String newDoc=doc.substring(0,47)+fix.getValue()+doc.substring(47+fix.getLength());
		assertEquals("        in DM.assocs $ DM.fromListWith (++) $ ((map (\\ (a, b) -> (a, [b])) cpkgs) ++\n    map (\\ (a, _) -> (a, [])) pkgs)",newDoc);
	}
	
	@Test
	public void testRedundantBracketSpace(){
		Suggestion sug=new Suggestion();
		sug.setPre(new CodeModificationText("  (map (\\ (a, b) -> (a, [b])) cpkgs) ++\n    (map (\\ (a, _) -> (a, [])) pkgs)"));
		sug.setPost(new CodeModificationText("  (map (\\ (a, b) -> (a, [b])) cpkgs) ++\n    map (\\ (a, _) -> (a, [])) pkgs"));
		sug.setLocation(new SourceLocation("src/Language/Haskell/BuildWrapper/Cabal.hs", 537, 48));
		sug.setMessage("Warning: Redundant bracket");
		
		String doc="        in DM.assocs $ DM.fromListWith (++) $ ((map (\\(a,b)->(a,[b])) cpkgs) ++ (map (\\(a,_)->(a,[])) pkgs)   )";
		HLintFix fix=HLintFixer.fix(doc, 47, sug);
		assertNotNull(fix);
		assertEquals(60,fix.getLength());
		String newDoc=doc.substring(0,47)+fix.getValue()+doc.substring(47+fix.getLength());
		assertEquals("        in DM.assocs $ DM.fromListWith (++) $ ((map (\\ (a, b) -> (a, [b])) cpkgs) ++\n    map (\\ (a, _) -> (a, [])) pkgs   )",newDoc);
	}
}
