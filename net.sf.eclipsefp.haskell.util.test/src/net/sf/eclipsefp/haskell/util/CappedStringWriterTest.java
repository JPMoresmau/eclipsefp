/** 
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.util;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;


/**
 * Tests for CappedStringWriter
 * @author JP Moresmau
 *
 */
public class CappedStringWriterTest {

	@Test
	public void testNormal() throws IOException{
		CappedStringWriter w=new CappedStringWriter(100);
		w.write("toto");
		w.write("titi");
		Assert.assertEquals("tototiti", w.toString());
	}
	
	@Test
	public void testClear() throws IOException{
		CappedStringWriter w=new CappedStringWriter(4);
		w.write("toto");
		w.clear();
		w.write("titi");
		Assert.assertEquals("titi", w.toString());
		w=new CappedStringWriter(4);
		w.write("toto");
		w.clear();
		w.write("tutu");
		w.write("titi");
		Assert.assertEquals("titi", w.toString());
	}
	
	
	@Test
	public void testNull() throws IOException{
		CappedStringWriter w=new CappedStringWriter(100);
		w.write("toto");
		w.write((String)null);
		assertEquals("toto", w.toString());
	}
	
	@Test
	public void testLimit() throws IOException{
		CappedStringWriter w=new CappedStringWriter(4);
		w.write("toto");
		w.write("titi");
		assertEquals("titi", w.toString());
	}
	
	@Test
	public void testLimitMiddle() throws IOException{
		CappedStringWriter w=new CappedStringWriter(6);
		w.write("toto");
		w.write("titi");
		assertEquals("titi", w.toString());
	}
	
	@Test
	public void testLimitMiddleAll() throws IOException{
		CappedStringWriter w=new CappedStringWriter(2);
		w.write("toto");
		w.write("titi");
		assertEquals("", w.toString());
	}
	
	@Test
	public void testPerf() throws IOException{
		CappedStringWriter w=new CappedStringWriter(100);
		StringBuilder sb=new StringBuilder();
		for (int a=0;a<100;a++){
			sb.append("a");
		}
		String s=sb.toString();
		assertEquals(100, s.length());
		for (int a=0;a<10000;a++){
			w.write(s);
		}
		assertEquals(s,w.toString());
	}
}
