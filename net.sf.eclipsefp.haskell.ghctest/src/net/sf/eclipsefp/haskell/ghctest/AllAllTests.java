package net.sf.eclipsefp.haskell.ghctest;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
	net.sf.eclipsefp.haskell.ghctest.AllTests.class,
	net.sf.eclipsefp.haskell.ghctest.lib.test.AllTests.class
})
public class AllAllTests {}
