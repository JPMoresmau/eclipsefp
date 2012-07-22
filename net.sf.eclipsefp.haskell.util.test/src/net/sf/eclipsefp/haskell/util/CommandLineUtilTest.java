/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.util;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

/**
 * test command line escaping
 * @author JP Moresmau
 *
 */
public class CommandLineUtilTest {

	@Test
	public void testSimple(){
		String s=CommandLineUtil.renderCommandLine(Arrays.asList("arg1","arg2"));
		Assert.assertEquals("arg1 arg2", s);
		String[] args=CommandLineUtil.parse(s);
		Assert.assertArrayEquals(new String[]{"arg1","arg2"}, args);
	}
	
	@Test
	public void testEmpties(){
		String s=CommandLineUtil.renderCommandLine(Arrays.asList("arg1","","arg2",null));
		Assert.assertEquals("arg1 arg2", s);
		String[] args=CommandLineUtil.parse(s);
		Assert.assertArrayEquals(new String[]{"arg1","arg2"}, args);
	}
	
	@Test
	public void testSpaces(){
		String s=CommandLineUtil.renderCommandLine(Arrays.asList("arg1","arg 2"));
		Assert.assertEquals("arg1 \"arg 2\"", s);
		String[] args=CommandLineUtil.parse(s);
		Assert.assertArrayEquals(new String[]{"arg1","arg 2"}, args);
	}
	
	@Test
	public void testSpacesAlreadyQuoted(){
		String s=CommandLineUtil.renderCommandLine(Arrays.asList("arg1","\"arg 2\""));
		Assert.assertEquals("arg1 \"arg 2\"", s);
		String[] args=CommandLineUtil.parse(s);
		Assert.assertArrayEquals(new String[]{"arg1","arg 2"}, args);
	}
	
	@Test
	public void testQuotes(){
		String s=CommandLineUtil.renderCommandLine(Arrays.asList("arg1","arg\"2"));
		Assert.assertEquals("arg1 \"arg\"\"2\"", s);
		String[] args=CommandLineUtil.parse(s);
		Assert.assertArrayEquals(new String[]{"arg1","arg\"2"}, args);
	}
	
	@Test
	public void testSpacesQuotes(){
		String s=CommandLineUtil.renderCommandLine(Arrays.asList("arg1","ar g\"2"));
		Assert.assertEquals("arg1 \"ar g\"\"2\"", s);
		String[] args=CommandLineUtil.parse(s);
		Assert.assertArrayEquals(new String[]{"arg1","ar g\"2"}, args);
	}
}
