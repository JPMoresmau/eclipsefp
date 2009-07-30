package net.sf.eclipsefp.haskell.core.halamo;

import static org.easymock.EasyMock.createMock;
import net.sf.eclipsefp.haskell.core.internal.util.TestHaskellProject;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithPreferences;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

public class HaskellModelManager_PDETest extends TestCaseWithPreferences {

	public void testCreatesModelsForNewProjects() {
		IWorkspace workspace = createMock(IWorkspace.class);
		IProject project = createMock(IProject.class);
		HaskellModelManager manager = new HaskellModelManager(workspace);

		IHaskellModel fstHalamo = manager.getModelFor(project);
		assertNotNull(fstHalamo);

		IHaskellModel sndHalamo = manager.getModelFor(project);
		assertNotNull(sndHalamo);

		assertSame(fstHalamo, sndHalamo);
	}

	public void testFetchesModelsForExistingProjects() throws CoreException {
		TestHaskellProject project = new TestHaskellProject("factorial", getCorePrefs());
		try {
			project.createSourceFile("Factorial.hs", "module Factorial where\n" +
					                                 "\n" +
					                                 "fac 0 = 1\n" +
					                                 "fac n = n * fac (n - 1)");

			HaskellModelManager mngr = new HaskellModelManager(
					ResourcesPlugin.getWorkspace());
			mngr.initialize();

			IHaskellModel prjModel = mngr.getModelFor(project.getPlatformProject());
			assertNotNull(prjModel);

			IModule module = prjModel.getModule("Factorial");
			assertNotNull(module);
			assertEquals(1, module.getDeclarations().length);
		} finally {
			project.destroy();
		}
	}

}
