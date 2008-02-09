package net.sf.eclipsefp.haskell.core.test.project;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import net.sf.eclipsefp.haskell.core.project.HaskellResource;

import org.eclipse.core.resources.IFile;

import junit.framework.TestCase;

public class HaskellResource_Test extends TestCase {
	
	private IFile haskellFile;
	private IFile literateHaskellFile;
	private IFile javaFile;

	@Override
	protected void setUp() {
		haskellFile = createNiceMock(IFile.class);
		literateHaskellFile = createNiceMock(IFile.class);
		javaFile = createNiceMock(IFile.class);
		expect(haskellFile.getName()).andReturn("Quicksort.hs");
		expect(literateHaskellFile.getName())
			.andReturn("MyModule.lhs")
			.anyTimes();
		expect(javaFile.getName()).andReturn("MyClass.java").anyTimes();
		replay(haskellFile, literateHaskellFile, javaFile);
	}

	public void testDotHsEndedFileIsHaskellFile() {
		assertTrue(new HaskellResource(haskellFile).isHaskellFile());
	}
	
	public void testDotLhsEndedFileIsHaskellFile() {
		assertTrue(new HaskellResource(literateHaskellFile).isHaskellFile());
	}

	public void testFileWithAnyOtherSuffixIsNotHaskellFile() {
		IFile chsFile = createNiceMock(IFile.class);
		IFile glhsFile = createNiceMock(IFile.class);
		expect(chsFile.getName()).andReturn("Something.chs");
		expect(glhsFile.getName()).andReturn("AnotherThing.glhs");
		replay(chsFile, glhsFile);
		
		assertFalse(new HaskellResource(javaFile).isHaskellFile());		
		assertFalse(new HaskellResource(chsFile).isHaskellFile());		
		assertFalse(new HaskellResource(glhsFile).isHaskellFile());		
	}
}
