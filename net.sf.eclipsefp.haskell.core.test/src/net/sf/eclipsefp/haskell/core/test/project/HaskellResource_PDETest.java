package net.sf.eclipsefp.haskell.core.test.project;

import net.sf.eclipsefp.haskell.core.project.HaskellResource;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithProject;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.NullProgressMonitor;

public class HaskellResource_PDETest extends TestCaseWithProject {

  private IFile haskellFile;
	private IFile literateHaskellFile;
	private IFile javaFile;


	// interface methods of TestCase
	////////////////////////////////

	@Override
	protected void setUp() throws Exception {
	  super.setUp();
		haskellFile = project.getFile( "Quicksort.hs" );
		literateHaskellFile = project.getFile( "MyModule.lhs" );
		javaFile = project.getFile("MyClass.java");
	}

  @Override
  protected void tearDown() throws Exception {
    if( project != null && project.exists() ) {
      project.delete( true, new NullProgressMonitor() );
    }
    super.tearDown();
  }


  // test case methods
  ////////////////////

	public void testDotHsEndedFileIsHaskellFile() {
    assertTrue( new HaskellResource( haskellFile ).isHaskellFile() );
  }

  public void testDotLhsEndedFileIsHaskellFile() {
    assertTrue( new HaskellResource( literateHaskellFile ).isHaskellFile() );
  }

	public void testFileWithAnyOtherSuffixIsNotHaskellFile() {
    IFile chsFile = project.getFile( "Something.chs" );
    IFile glhsFile = project.getFile( "AnotherThing.glhs" );
    assertFalse( new HaskellResource( javaFile ).isHaskellFile() );
    assertFalse( new HaskellResource( chsFile ).isHaskellFile() );
    assertFalse( new HaskellResource( glhsFile ).isHaskellFile() );
  }
}
