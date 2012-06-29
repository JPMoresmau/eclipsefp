/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.style;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import net.sf.eclipsefp.haskell.style.stylishhaskell.SHConfiguration;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHImports.SHImportAlign;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHPragmas.SHPragmaStyle;
import net.sf.eclipsefp.haskell.style.stylishhaskell.StylishHaskell;

import org.junit.Test;

/**
 * Tests the Stylish Haskell configuration, especially YAML I/O
 * @author JP Moresmau
 *
 */
public class SHConfigurationTest {

	
	@Test
	public void testDefault(){
		SHConfiguration def=new SHConfiguration();
		assertNull(def.getTabs());
		assertNull(def.getUnicode());
		assertNotNull(def.getImports());
		assertEquals(SHImportAlign.GLOBAL, def.getImports().getAlign());
		assertNotNull(def.getPragmas());
		assertTrue(def.getPragmas().isRemoveRedundant());
		assertEquals(SHPragmaStyle.VERTICAL,def.getPragmas().getStyle());
		assertNotNull(def.getTrailingWhitespace());
	}
	
	@Test
	public void testIODefault() throws IOException{
		SHConfiguration def=new SHConfiguration();
		InputStream is=getClass().getResourceAsStream("stylish-haskell-default.yaml");
		assertNotNull(is);
		SHConfiguration defRead=StylishHaskell.load(is);
		assertEquals(def,defRead);
		ByteArrayOutputStream baos=new ByteArrayOutputStream();
		StylishHaskell.save(defRead, baos);
		SHConfiguration defRead2=StylishHaskell.load(new ByteArrayInputStream(baos.toByteArray()));
		assertEquals(def,defRead2);
	}
	
	@Test
	public void testIOFull() throws IOException{
		InputStream is=getClass().getResourceAsStream("stylish-haskell-full.yaml");
		assertNotNull(is);
		SHConfiguration confRead=StylishHaskell.load(is);
		assertNotNull(confRead.getTabs());
		assertEquals(4,confRead.getTabs().getSpaces());
		assertNotNull(confRead.getUnicode());
		assertTrue(confRead.getUnicode().isUnicodePragmas());
		
		assertNotNull(confRead.getImports());
		assertEquals(SHImportAlign.GROUP, confRead.getImports().getAlign());
		assertNotNull(confRead.getPragmas());
		assertFalse(confRead.getPragmas().isRemoveRedundant());
		assertEquals(SHPragmaStyle.COMPACT,confRead.getPragmas().getStyle());
		assertNotNull(confRead.getTrailingWhitespace());
		
		ByteArrayOutputStream baos=new ByteArrayOutputStream();
		StylishHaskell.save(confRead, baos);
		SHConfiguration confRead2=StylishHaskell.load(new ByteArrayInputStream(baos.toByteArray()));
		assertEquals(confRead,confRead2);
	}
}
