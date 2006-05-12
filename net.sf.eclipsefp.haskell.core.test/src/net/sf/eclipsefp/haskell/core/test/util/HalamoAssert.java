package net.sf.eclipsefp.haskell.core.test.util;

import java.util.List;

import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;
import junit.framework.Assert;

/**
 * Convenience assertions for the Haskell Language Model tests.
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class HalamoAssert extends Assert {
	
	public static <T extends IHaskellLanguageElement> T
	    assertContains(String name, List<T> elements)
	{
		for (T module : elements) {
			if (name.equals(module.getName())) {
				return module;
			}
		}
		fail("Element " + name + " not found");
		return null;
	}


}
