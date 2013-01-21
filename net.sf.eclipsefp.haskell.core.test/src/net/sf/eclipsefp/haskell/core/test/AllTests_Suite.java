// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.test;

import junit.framework.JUnit4TestAdapter;
import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersionTest;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalModelTest;
import net.sf.eclipsefp.haskell.core.cabalmodel.JSONDescriptionHelperTest;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoaderTest;
import net.sf.eclipsefp.haskell.core.parser.ParserUtilsTest;
import net.sf.eclipsefp.haskell.core.util.GHCiSyntaxTest;

public class AllTests_Suite {

	public static Test suite() {
		TestSuite suite = new TestSuite();
		suite.addTestSuite( CabalModelTest.class );
		suite.addTestSuite( ParserUtilsTest.class );
		suite.addTestSuite( GHCiSyntaxTest.class );

		suite.addTest( new JUnit4TestAdapter(CabalPackageVersionTest.class ));

		suite.addTest( new JUnit4TestAdapter(CabalModelTest.class ));
		suite.addTest( new JUnit4TestAdapter(PackageDescriptionLoaderTest.class ));
    suite.addTest( new JUnit4TestAdapter(JSONDescriptionHelperTest.class) );
		return suite;
	}

}
