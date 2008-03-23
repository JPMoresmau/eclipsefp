// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.project;

import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.internal.project.IManipulateCabalFile.Accessor;
import net.sf.eclipsefp.haskell.core.internal.project.IManipulateCabalFile.Mutator;
import org.eclipse.core.runtime.CoreException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

/** test cases for manipulating Cabal files (happens via Cohatoe and the
  * Cabal API.
  *
  * @author Leif Frenzel
  */
public class ManipulateCabalFile_PDETest extends TestCase {

  public void testManipulateLicense() throws CoreException {
    // test getting from simple file
    String buf = "Name: Bla\nVersion: 1\nLicense: BSD3\n";
    assertEquals( "BSD3", getManipulator().get( buf, Accessor.GET_LICENSE ) );

    buf = "Name: Bla\nVersion: 1\n";
    assertEquals( "AllRightsReserved",
                  getManipulator().get( buf, Accessor.GET_LICENSE ) );

    // test setting - contains new license
    String newBuf = getManipulator().set( buf, Mutator.SET_LICENSE, "BSD3" );
    assertContains( newBuf, "license: BSD3" );

    // test setting bogus thing - doesn't break, does still contain old value
    newBuf = getManipulator().set( newBuf, Mutator.SET_LICENSE, "XYZ" );
    assertContains( newBuf, "license: BSD3" );
  }

  public void testManipulateName() throws CoreException {
    // test getting from simple file
    String buf = "Name: Bla\nVersion: 1\nLicense: BSD3\n";
    assertEquals( "Bla", getManipulator().get( buf, Accessor.GET_NAME ) );

    // test setting
    String newBuf = getManipulator().set( buf, Mutator.SET_NAME, "Blubb" );
    assertContains( newBuf, "name: Blubb" );
  }


  // helping methods
  //////////////////

  private void assertContains( final String content, final String candidate ) {
    assertTrue( content.indexOf( candidate ) != -1 );
  }

  private IManipulateCabalFile getManipulator() {
    CohatoeServer server = CohatoeServer.getInstance();
    return server.createFunction( IManipulateCabalFile.class );
  }
}
