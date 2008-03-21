// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.test.internal;

import junit.framework.Test;
import junit.framework.TestSuite;
import net.sf.eclipsefp.haskell.debug.ui.internal.launch.ghci.GhciLaunchOperationDelegate_PDETest;

/** <p>collects all PDE tests in this plugin.</p>
  *
  * @author Leif Frenzel
  */
public class AllTests_PDESuite {

  public static Test suite() {
    TestSuite suite = new TestSuite();
    suite.addTestSuite( GhciLaunchOperationDelegate_PDETest.class );
    return suite;
  }
}
