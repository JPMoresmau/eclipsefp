package net.sf.eclipsefp.haskell.core.test.expressions;

import static org.easymock.EasyMock.createNiceMock;
import static org.easymock.EasyMock.expect;
import static org.easymock.EasyMock.replay;
import junit.framework.TestCase;
import net.sf.eclipsefp.haskell.core.expressions.HaskellPropertyTester;
import org.eclipse.core.resources.IFile;

public class HaskellPropertyTester_PDETest extends TestCase {

	private HaskellPropertyTester tester;
	private IFile haskellFile;
	private IFile literateHaskellFile;
	private IFile javaFile;

	@Override
	protected void setUp() {
		haskellFile = createNiceMock(IFile.class);
		literateHaskellFile = createNiceMock(IFile.class);
		javaFile = createNiceMock(IFile.class);
		expect(haskellFile.getName()).andReturn("Quicksort.hs");
		expect(haskellFile.getFileExtension()).andReturn("hs");
		expect(literateHaskellFile.getName())
			.andReturn("MyModule.lhs")
			.anyTimes();
		expect(literateHaskellFile.getFileExtension()).andReturn("lhs").anyTimes();
		expect(javaFile.getName()).andReturn("MyClass.java").anyTimes();
		replay(haskellFile, literateHaskellFile, javaFile);

		tester = new HaskellPropertyTester();
	}

	public void testDetectsTraditionalHaskellFile() {
		assertTrue(tester.test(haskellFile , "isHaskellFile", null, null));
	}

	public void testDetectsLiterateHaskellFile() {
		assertTrue(tester.test(literateHaskellFile, "isHaskellFile", null, null));
	}

	public void testRejectsNonHaskellFile() {
		assertFalse(tester.test(javaFile, "isHaskellFile", null, null));
	}
}
