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

  public void testGetAllSourceDirs() throws Exception {
    // none
    String buf = "Name: Bla\nVersion: 1\n";
    assertEquals( 0, getManipulator().getAllSourceDirs( buf ).length );

    // one (library)
    buf = "Name: Bla\nVersion: 1\n\nExecutable a\n  hs-source-dirs: src\nMain-is: A.hs\n  Build-depends: base\n";
    String[] srcDirs = getManipulator().getAllSourceDirs( buf );
    assertEquals( 1, srcDirs.length );
    assertEquals( "src", srcDirs[ 0 ] );

    // path
    buf = "Name: Bla\nVersion: 1\nhs-source-dirs: some/Path/src\n";
    srcDirs = getManipulator().getAllSourceDirs( buf );
    assertEquals( 1, srcDirs.length );
    assertEquals( "some/Path/src", srcDirs[ 0 ] );

    // multiple
    buf = "Name: Bla\nVersion: 1\nhs-source-dirs: src1, src2\n";
    srcDirs = getManipulator().getAllSourceDirs( buf );
    assertEquals( 2, srcDirs.length );
    assertEquals( "src1", srcDirs[ 0 ] );
    assertEquals( "src2", srcDirs[ 1 ] );

    // TODO lf multiple in multiple libs/exes/stanzas
  }


//  private String createTestCabalFile() {
//    return   "Name:            cohatoe-plugin\n"
//           + "Version:         1.103.0\n"
//           + "Build-depends:   base, cohatoe-api, haskell98\n"
//           + "Hs-Source-Dirs:  ../../hs-src\n"
//           + "Ghc-options:     -Wall -package-name main\n"
//           + "Exposed-modules: MarkOccurrences";
//    return   "Name:           TestPackage\n"
//           + "Version:        0.0\n"
//           + "Cabal-Version:  >= 1.2\n"
//           + "License:        BSD3\n"
//           + "Author:         Angela Author\n"
//           + "Synopsis:       Small package with two programs\n"
//           + "Build-Type:     Simple\n"
//           + "\n"
//           + "Executable program1\n"
//           + "  Build-Depends:  HUnit\n"
//           + "  Main-Is:        Main.hs\n"
//           + "  Hs-Source-Dirs: prog1\n"
//           + "  \n"
//           + "Executable program2\n"
//           + "  Main-Is:        Main.hs\n"
//           + "  Build-Depends:  HUnit\n"
//           + "  Hs-Source-Dirs: prog2\n"
//           + "  Other-Modules:  Utils\n";
//  }

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
