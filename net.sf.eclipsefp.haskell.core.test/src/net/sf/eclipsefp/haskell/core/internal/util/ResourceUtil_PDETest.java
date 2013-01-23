package net.sf.eclipsefp.haskell.core.internal.util;

import net.sf.eclipsefp.haskell.core.test.TestCaseWithProject;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;

public class ResourceUtil_PDETest extends TestCaseWithProject {

	public void testExtractModuleNameFromUnliterateFileName() {
    assertEquals( "MyModule", ResourceUtil.getModuleName( "MyModule.hs" ) );
  }

  public void testExtractModuleNameFromLiterateFileName() {
    assertEquals( "MyModule", ResourceUtil.getModuleName( "MyModule.lhs" ) );
  }

  public void testGetSourceDirRelativeName() throws Exception {

    IFile file = null;

      file = project.getFile( "A.hs" );
      assertEquals(file.getFullPath(),ResourceUtil.getSourceFolderRelativeName( file ));


    file = project.getFile( "src/A.hs" );
    assertEquals( "A.hs", ResourceUtil.getSourceFolderRelativeName( file).toPortableString() );

    file = project.getFile( "src/Bla/A.hs" );
    assertEquals( "Bla/A.hs",
                  ResourceUtil.getSourceFolderRelativeName( file ).toPortableString() );

    file = project.getFile( "src/Some/Long/Path/A.hs" );
    assertEquals( "Some/Long/Path/A.hs",
                  ResourceUtil.getSourceFolderRelativeName( file ).toPortableString() );

     file = project.getFile( "test/Some/Path/A.hs" );
     assertEquals(file.getFullPath(),ResourceUtil.getSourceFolderRelativeName( file ));

  }
}
