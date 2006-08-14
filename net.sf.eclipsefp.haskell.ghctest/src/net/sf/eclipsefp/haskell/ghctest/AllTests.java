package net.sf.eclipsefp.haskell.ghctest;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({
	SuccessfulOutputTest.class,
	ErrorOutputTest.class
})
public class AllTests {}
