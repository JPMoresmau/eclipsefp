// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.project;

import java.util.Set;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/** <p>general functionality test: we don't test the functionality of a
  * particular class here, but the general behaviour of the Haskell project
  * model.
  *
  * @author Leif Frenzel
  */
public class ProjectModel_PDETest extends TestCaseWithProject {

  @Override
  protected void tearDown() throws Exception {
    super.tearDown();
    HaskellProjectManager.clear();
  }


  // test cases
  /////////////

  // create new project -> must have a Cabal file with default src/ as
  // source folder
  public void testSourceFolderInNewProject() throws Exception {
    IHaskellProject hsProject = HaskellProjectManager.get( project );
    Set<IPath> sourcePaths = hsProject.getSourcePaths();
    assertEquals( 1, sourcePaths.size() );
    assertEquals( new Path( "src" ), sourcePaths.iterator().next() );
  }

  // monitoring changes on the cabal file

  // TODO lf not yet fully implemented

//  public void testRenamedSourceFolder() throws Exception {
//    String name = project.getName();
//    IFile file = project.getFile( name + ".cabal" );
//    String content = "name: " + name + "\nVersion: 1\nhs-source-dirs: src1\n";
//    InputStream is = new ByteArrayInputStream( content.getBytes() );
//    file.setContents( is, true, false, null );
//    waitForAutoBuild();
//
//    IHaskellProject hsProject = HaskellProjectManager.get( project );
//    Set<IPath> sourcePaths = hsProject.getSourcePaths();
//    assertEquals( 1, sourcePaths.size() );
//    assertEquals( new Path( "src1" ), sourcePaths.iterator().next() );
//  }
//
//  public void testAddedSourceFolder() throws Exception {
//    String name = project.getName();
//    IFile file = project.getFile( name + ".cabal" );
//    String content = "name: " + name + "\nVersion: 1\nhs-source-dirs: src1, src\n";
//    InputStream is = new ByteArrayInputStream( content.getBytes() );
//    file.setContents( is, true, false, null );
//    waitForAutoBuild();
//
//    IHaskellProject hsProject = HaskellProjectManager.get( project );
//    Set<IPath> sourcePaths = hsProject.getSourcePaths();
//    assertEquals( 2, sourcePaths.size() );
//    assertEquals( new Path( "src1" ), sourcePaths.iterator().next() );
//    assertEquals( new Path( "src2" ), sourcePaths.iterator().next() );
//  }
//
//  // if the hs-source-dirs attribute is deleted, it defaults to "."
//  // for us this means we expect an empty list of source folders, the
//  // project counts as source folder
//  public void testDeletedSourceFolder() throws Exception {
//    String name = project.getName();
//    IFile file = project.getFile( name + ".cabal" );
//    String content = "name: " + name + "\nVersion: 1\n";
//    InputStream is = new ByteArrayInputStream( content.getBytes() );
//    file.setContents( is, true, false, null );
//    waitForAutoBuild();
//
//    IHaskellProject hsProject = HaskellProjectManager.get( project );
//    Set<IPath> sourcePaths = hsProject.getSourcePaths();
//    assertEquals( 0, sourcePaths.size() );
//    assertEquals( project, hsProject.getSourceFolder() );
//  }

  // TODO lf test ignoring changes on non-Haskell projects
}
