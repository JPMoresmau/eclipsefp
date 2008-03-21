package net.sf.eclipsefp.haskell.core.internal.util;

import java.util.Collection;
import junit.framework.Assert;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellLanguageElement;

/**
 * Convenience assertions for the Haskell Language Model tests.
 *
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class HalamoAssert extends Assert {

	public static <T extends IHaskellLanguageElement> T
	    assertContains(final String name, final Collection<T> modules)
	{
		for (T module : modules) {
			if (name.equals(module.getName())) {
				return module;
			}
		}
		fail("Element " + name + " not found");
		return null;
	}

}
