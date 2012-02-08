/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.hlint;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
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
		assertTrue(fix.isFullMatch());
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
		assertTrue(fix.isFullMatch());
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
		assertTrue(fix.isFullMatch());
		assertEquals(60,fix.getLength());
		String newDoc=doc.substring(0,47)+fix.getValue()+doc.substring(47+fix.getLength());
		assertEquals("        in DM.assocs $ DM.fromListWith (++) $ ((map (\\ (a, b) -> (a, [b])) cpkgs) ++\n    map (\\ (a, _) -> (a, [])) pkgs   )",newDoc);
	}
	
	@Test
	public void testDiscardComment(){
		Suggestion sug=new Suggestion();
		sug.setPre(new CodeModificationText("do copyFile src tgt"));
		sug.setPost(new CodeModificationText("copyFile src tgt"));
		sug.setLocation(new SourceLocation("src/Language/Haskell/BuildWrapper/Cabal.hs", 537, 48));
		sug.setMessage("Warning: Redundant do");
		String doc="copyFileFull src tgt=do\n    --createDirectoryIfMissing True (takeDirectory tgt)\n    --putStrLn tgt\n    copyFile src tgt";
		HLintFix fix=HLintFixer.fix(doc, 21, sug);
		assertNotNull(fix);
		assertTrue(fix.isFullMatch());
		assertEquals(98,fix.getLength());
		String newDoc=doc.substring(0,21)+fix.getValue()+doc.substring(21+fix.getLength());
		assertEquals("copyFileFull src tgt=copyFile src tgt",newDoc);
	}
	
	@Test
	public void testAddSpace(){
		Suggestion sug=new Suggestion();
		sug.setPre(new CodeModificationText("(cbi)"));
		sug.setPost(new CodeModificationText("cbi"));
		sug.setLocation(new SourceLocation("src/Language/Haskell/BuildWrapper/Cabal.hs", 537, 48));
		sug.setMessage("Warning: Redundant bracket");
		String doc="case mcbi of\n    Just(cbi)->do";
		HLintFix fix=HLintFixer.fix(doc, 21, sug);
		assertNotNull(fix);
		assertTrue(fix.isFullMatch());
		assertEquals(5,fix.getLength());
		String newDoc=doc.substring(0,21)+fix.getValue()+doc.substring(21+fix.getLength());
		assertEquals("case mcbi of\n    Just cbi->do",newDoc);
	}
	
	@Test
	public void testExtraBrackets(){
		String t0="case ls of\n    (loc : (line : (col : []))) -> (Just $\n                                      BWNote BWError (dropWhile isSpace aft)\n                                        (BWLocation loc (read line) (read col)))\n    _ -> Nothing";	
		String t1="case ls of\n    (loc : (line : (col : []))) -> Just $\n                                     BWNote BWError (dropWhile isSpace aft)\n                                       (BWLocation loc (read line) (read col))\n    _ -> Nothing";
		Suggestion sug=new Suggestion();
		sug.setPre(new CodeModificationText(t0));
		sug.setPost(new CodeModificationText(t1));
		sug.setLocation(new SourceLocation("src/Language/Haskell/BuildWrapper/Cabal.hs", 338, 28));
		sug.setMessage("Warning: Redundant bracket");
		String doc="                        in case ls of\n                                (loc:line:col:[])-> (Just $ BWNote BWError (dropWhile isSpace aft) (BWLocation loc (read line) (read col)))\n                                _ -> Nothing";
		HLintFix fix=HLintFixer.fix(doc, 27, sug);
		assertTrue(fix.isFullMatch());
		assertNotNull(fix);
		assertEquals(195,fix.getLength());
		String newDoc=doc.substring(0,27)+fix.getValue()+doc.substring(27+fix.getLength());
		assertEquals("                        in case ls of\n    (loc : (line : (col : []))) -> Just $\n                                     BWNote BWError (dropWhile isSpace aft)\n                                       (BWLocation loc (read line) (read col))\n    _ -> Nothing",newDoc);
	}
	
	@Test
	public void testNoMatch(){
		Suggestion sug=new Suggestion();
		sug.setPre(new CodeModificationText("(aaa)"));
		sug.setPost(new CodeModificationText("cbi"));
		sug.setLocation(new SourceLocation("src/Language/Haskell/BuildWrapper/Cabal.hs", 537, 48));
		sug.setMessage("Warning: Redundant bracket");
		String doc="case mcbi of\n    Just(cbi)->do";
		HLintFix fix=HLintFixer.fix(doc, 21, sug);
		assertNotNull(fix);
		assertFalse(fix.isFullMatch());
	}

}


