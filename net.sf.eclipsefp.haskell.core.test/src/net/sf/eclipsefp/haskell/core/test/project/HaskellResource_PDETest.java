package net.sf.eclipsefp.haskell.core.test.project;

import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.project.HaskellResource;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;

public class HaskellResource_PDETest extends TestCase {

  private IProject project;

  private IFile haskellFile;
	private IFile literateHaskellFile;
	private IFile javaFile;


	// interface methods of TestCase
	////////////////////////////////

	@Override
	protected void setUp() throws Exception {
    project = createProject();
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


	// helping functions
	////////////////////

	 private IProject createProject() throws CoreException {
    IWorkspace ws = ResourcesPlugin.getWorkspace();
    IProject result = ws.getRoot().getProject( "testProject" );
    IProjectDescription description = ws.newProjectDescription( "testProject" );
    description.setNatureIds( new String[] { HaskellNature.NATURE_ID } );
    result.create( description, new NullProgressMonitor() );
    result.open( new NullProgressMonitor() );
    return result;
  }
}
