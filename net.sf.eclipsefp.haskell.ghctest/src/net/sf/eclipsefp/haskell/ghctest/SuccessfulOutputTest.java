package net.sf.eclipsefp.haskell.ghctest;

import static net.sf.eclipsefp.haskell.ghctest.lib.Assert.*;

import org.junit.Test;

public class SuccessfulOutputTest {
	
	@Test public void flagVersionSaysVersionNumber() {
		assertCommandMatches("The Glorious Glasgow Haskell Compilation System, version",
				             "ghc --version");
	}

}
