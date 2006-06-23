package net.sf.eclipsefp.haskell.core.test.halamo;

import java.io.StringBufferInputStream;

import net.sf.eclipsefp.haskell.core.halamo.IHaskellModel;
import net.sf.eclipsefp.haskell.core.halamo.IModule;
import net.sf.eclipsefp.haskell.core.halamo.ProjectChangeMonitor;
import net.sf.eclipsefp.test.util.haskell.HaskellProject_PDETestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

import static org.easymock.EasyMock.*;

/**
 * The ResourceChangeMonitor is responsible for keeping the language model
 * up-to-date. Here we check if it is calling the expected methods for updating
 * the model.
 * 
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class ProjectChangeMonitor_PDETestCase extends HaskellProject_PDETestCase {
	
	private IHaskellModel fLanguageModel;
	private IResourceChangeListener fMonitor;

	@Override
	protected void setUpMore() throws Exception {
		fLanguageModel = createMock(IHaskellModel.class);
		fMonitor = new ProjectChangeMonitor(getLanguageModel());

		getWorkspace().addResourceChangeListener(getMonitor());
	}

	public void testAddModule() throws CoreException {
		getLanguageModel().putModule((IModule) anyObject());
		expectLastCall().atLeastOnce();
		replay(getLanguageModel());
	
		createSourceFile("module QuickSort where\n\n", "QuickSort.hs");
		
		verify(getLanguageModel());
	}
	
	@SuppressWarnings("deprecation")
	public void testChangeModule() throws CoreException {
		getLanguageModel().putModule((IModule) anyObject());
		expectLastCall().times(2);
		replay(getLanguageModel());
		
		IFile file = createSourceFile("module QuickSort where\n\n", "QuickSort.hs");
		
		file.setContents(new StringBufferInputStream("module QuickSort where\n\n"), true, false, null);
		
		verify(getLanguageModel());
	}
	
	public void testRemoveModule() throws CoreException {
		getLanguageModel().putModule((IModule) anyObject());
		getLanguageModel().removeModule("QuickSort");
		replay(getLanguageModel());

		IFile file = createSourceFile("module QuickSort where\n\n", "QuickSort.hs");

		file.delete(true, null);
		
		verify(getLanguageModel());
	}
	
	public void testDoNotRemoveModuleWhenDeletingNonHaskellFile() throws CoreException {
		getLanguageModel().putModule((IModule) anyObject());
		replay(getLanguageModel());

		createSourceFile("module ReadMe where\n\n", "Readme.hs");
		IFile nonHaskellfile = createSourceFile("read this!", "Readme.tx");
		
		nonHaskellfile.delete(true, null);

		verify(getLanguageModel());		
	}
	
	//TODO remove module when using literate haskell
	
	@Override
	protected void doTearDown() throws Exception {
		getWorkspace().removeResourceChangeListener(getMonitor());
	}

	private IWorkspace getWorkspace() {
		return ResourcesPlugin.getWorkspace();
	}

	private IHaskellModel getLanguageModel() {
		return fLanguageModel;
	}

	private IResourceChangeListener getMonitor() {
		return fMonitor;
	}
	
}
