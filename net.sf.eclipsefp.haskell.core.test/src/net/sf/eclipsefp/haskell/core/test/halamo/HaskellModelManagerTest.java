package net.sf.eclipsefp.haskell.core.test.halamo;

import net.sf.eclipsefp.haskell.core.halamo.HaskellLanguageModel;
import net.sf.eclipsefp.haskell.core.halamo.HaskellModelManager;

import org.eclipse.core.resources.IProject;

import junit.framework.TestCase;

import static org.easymock.EasyMock.*;

public class HaskellModelManagerTest extends TestCase {
	
	public void testCreatesModelsForNewProjects() {
		IProject project = createMock(IProject.class);
		HaskellModelManager manager = HaskellModelManager.getInstance();
		
		HaskellLanguageModel fstHalamo = manager.getModelFor(project);
		assertNotNull(fstHalamo);
		
		HaskellLanguageModel sndHalamo = manager.getModelFor(project);
		assertNotNull(sndHalamo);
		
		assertSame(fstHalamo, sndHalamo);
	}

}
