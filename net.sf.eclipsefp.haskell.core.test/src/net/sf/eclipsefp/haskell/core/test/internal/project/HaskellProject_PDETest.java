// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.test.internal.project;

import net.sf.eclipsefp.haskell.core.internal.project.HaskellProject;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;


public class HaskellProject_PDETest extends TestCaseWithProject {

  public void testSingleSourcePath() {
    HaskellProject hp = new HaskellProject( project );
    assertEquals( 0, hp.getSourcePaths().size() );
    assertEquals( project, hp.getSourceFolder() );

    hp.addSourcePath( "src" );
    assertEquals( 1, hp.getSourcePaths().size() );
    assertEquals( project.getFolder( "src" ), hp.getSourceFolder() );

    assertTrue( ResourceUtil.isSourceFolder( project.getFolder( "src" ) ) );
    assertFalse( ResourceUtil.isSourceFolder( project.getFolder( "crs" ) ) );
    assertFalse( ResourceUtil.isSourceFolder( project.getFolder( "src/bla" ) ) );
  }

  public void testMultipleSourcePaths() {
    HaskellProject hp = new HaskellProject( project );
    assertEquals( 0, hp.getSourcePaths().size() );
    assertEquals( project, hp.getSourceFolder() );

    hp.addSourcePath( "src" );
    hp.addSourcePath( "test" );
    assertEquals( 2, hp.getSourcePaths().size() );
    // TODO lf we should really get two here
//    assertEquals( project.getFolder( "src" ), hp.getSourceFolder() );

    assertTrue( ResourceUtil.isSourceFolder( project.getFolder( "src" ) ) );
    assertTrue( ResourceUtil.isSourceFolder( project.getFolder( "test" ) ) );
    assertFalse( ResourceUtil.isSourceFolder( project.getFolder( "crs" ) ) );
  }
}
