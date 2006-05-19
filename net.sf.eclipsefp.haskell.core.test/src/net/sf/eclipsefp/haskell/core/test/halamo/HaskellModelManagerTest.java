package net.sf.eclipsefp.haskell.core.test.halamo;

import net.sf.eclipsefp.haskell.core.halamo.HaskellModelManager;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellModel;

import org.eclipse.core.resources.IProject;

import junit.framework.TestCase;

import static org.easymock.EasyMock.*;

public class HaskellModelManagerTest extends TestCase {
	
	public void testCreatesModelsForNewProjects() {
		IProject project = createMock(IProject.class);
		HaskellModelManager manager = HaskellModelManager.getInstance();
		
		IHaskellModel fstHalamo = manager.getModelFor(project);
		assertNotNull(fstHalamo);
		
		IHaskellModel sndHalamo = manager.getModelFor(project);
		assertNotNull(sndHalamo);
		
		assertSame(fstHalamo, sndHalamo);
	}

}
