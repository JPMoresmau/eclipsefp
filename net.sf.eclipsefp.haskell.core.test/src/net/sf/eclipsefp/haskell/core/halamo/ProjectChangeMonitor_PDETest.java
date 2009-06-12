package net.sf.eclipsefp.haskell.core.halamo;

import static org.easymock.EasyMock.anyObject;
import static org.easymock.EasyMock.createMock;
import static org.easymock.EasyMock.expectLastCall;
import static org.easymock.EasyMock.replay;
import static org.easymock.EasyMock.verify;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import net.sf.eclipsefp.haskell.core.internal.project.HaskellProject_PDETestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

/**
 * The ResourceChangeMonitor is responsible for keeping the language model
 * up-to-date. Here we check if it is calling the expected methods for updating
 * the model.
 *
 * @author Thiago Arrais - thiago.arrais@gmail.com
 */
public class ProjectChangeMonitor_PDETest extends HaskellProject_PDETestCase {

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

	public void testChangeModule() throws CoreException {
		getLanguageModel().putModule((IModule) anyObject());
		expectLastCall().times(2);
		replay(getLanguageModel());

		IFile file = createSourceFile("module QuickSort where\n\n", "QuickSort.hs");
		byte[] bytes = "module QuickSort where\n\n".getBytes();
    InputStream is = new ByteArrayInputStream( bytes );
		file.setContents( is, true, false, null );

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

	public void testRemoveLiterateModule() throws CoreException {
		getLanguageModel().putModule((IModule) anyObject());
		getLanguageModel().removeModule("Factorial");
		replay(getLanguageModel());

		IFile file = createSourceFile("> module Factorial where\n\n", "Factorial.lhs");

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
