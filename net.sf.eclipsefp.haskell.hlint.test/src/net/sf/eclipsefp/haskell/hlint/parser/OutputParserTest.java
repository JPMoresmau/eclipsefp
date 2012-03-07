package net.sf.eclipsefp.haskell.hlint.parser;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.util.List;

import net.sf.eclipsefp.haskell.hlint.CodeModificationText;
import net.sf.eclipsefp.haskell.hlint.SourceLocation;
import net.sf.eclipsefp.haskell.hlint.Suggestion;

import org.junit.Test;

/**
 * Tests the hlint output parser
 * @author JP Moresmau
 *
 */
public class OutputParserTest {

	@Test
	public void testWindowsLongPath() throws Exception{
		//File f=new File("test.txt");
		InputStream is=OutputParserTest.class.getResourceAsStream("testW.txt");
		assertNotNull(is);
		OutputParser p=new OutputParser(is);
		List<Suggestion> sugs=p.suggestions();
		assertNotNull(sugs);
		assertEquals(3, sugs.size());
		Suggestion sug=sugs.get(0);
		assertEquals("Unused LANGUAGE pragma",sug.getMessage());
		SourceLocation sl=sug.getLocation();
		assertNotNull(sl);
		assertEquals("D:\\dev\\haskell\\jp-github\\runtime-New_configuration\\scion\\server\\Scion\\Server\\Protocol.hs",sl.getFilename());
		assertEquals(1,sl.getLine());
		assertEquals(1,sl.getColumn());
		assertTrue(sug.getPre() instanceof CodeModificationText);
		assertEquals("{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances,\n  PatternGuards #-}",((CodeModificationText)sug.getPre()).getText());
		assertTrue(sug.getPost() instanceof CodeModificationText);
		assertEquals("{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances #-}",((CodeModificationText)sug.getPost()).getText());
	}

	@Test
	public void testUnixLongPath() throws Exception{
		//File f=new File("test.txt");
		InputStream is=OutputParserTest.class.getResourceAsStream("testU.txt");
		assertNotNull(is);
		OutputParser p=new OutputParser(is);
		List<Suggestion> sugs=p.suggestions();
		assertNotNull(sugs);
		assertEquals(3, sugs.size());
		Suggestion sug=sugs.get(0);
		assertEquals("Unused LANGUAGE pragma",sug.getMessage());
		SourceLocation sl=sug.getLocation();
		assertNotNull(sl);
		assertEquals("/dev/haskell/jp-github/runtime-New_configuration/scion/server/Scion/Server/Protocol.hs",sl.getFilename());
		assertEquals(1,sl.getLine());
		assertEquals(1,sl.getColumn());
		assertTrue(sug.getPre() instanceof CodeModificationText);
		assertEquals("{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances,\n  PatternGuards #-}",((CodeModificationText)sug.getPre()).getText());
		assertTrue(sug.getPost() instanceof CodeModificationText);
		assertEquals("{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances #-}",((CodeModificationText)sug.getPost()).getText());
	}

	@Test
	public void testWithNote() throws Exception {
		InputStream is=OutputParserTest.class.getResourceAsStream("note.txt");
		assertNotNull(is);
		OutputParser p=new OutputParser(is);
		List<Suggestion> sugs=p.suggestions();
		assertNotNull(sugs);
		assertEquals(4, sugs.size());
		Suggestion sug=sugs.get(0);
		assertEquals("Use Control.Exception.catch",sug.getMessage());
		SourceLocation sl=sug.getLocation();
		assertNotNull(sl);
		assertEquals("D:\\dev\\haskell\\jp-github\\runtime-New_configuration\\P1\\.dist-scion\\build\\autogen\\Paths_P1.hs",sl.getFilename());
		assertEquals(21,sl.getLine());
		assertEquals(13,sl.getColumn());
		assertTrue(sug.getPre() instanceof CodeModificationText);
		assertEquals("catch",((CodeModificationText)sug.getPre()).getText());
		assertTrue(sug.getPost() instanceof CodeModificationText);
		assertEquals("Control.Exception.catch",((CodeModificationText)sug.getPost()).getText());

	}
	
	@Test
	public void testYesod() throws Exception {
		InputStream is=OutputParserTest.class.getResourceAsStream("yesod.txt");
		assertNotNull(is);
		OutputParser p=new OutputParser(is);
		List<Suggestion> sugs=p.suggestions();
		assertNotNull(sugs);
		assertEquals(0, sugs.size());
	}
}
