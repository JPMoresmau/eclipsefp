package net.sf.eclipsefp.haskell.ui.test.editor.codeassist;

import static org.easymock.EasyMock.expect; 
import static org.easymock.EasyMock.createMock; 
import static org.easymock.EasyMock.replay; 
import static org.easymock.EasyMock.verify; 

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;

import net.sf.eclipsefp.haskell.core.codeassist.HaskellCompletionContext;
import net.sf.eclipsefp.haskell.core.halamo.HaskellLanguageModel;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellModel;
import net.sf.eclipsefp.haskell.core.halamo.IHaskellModelManager;
import net.sf.eclipsefp.haskell.ui.editor.codeassist.WorkbenchHaskellCompletionContext;
import net.sf.eclipsefp.test.util.haskell.TestHaskellProject;
import junit.framework.TestCase;

public class WorkbenchHaskellCompletionContextTest extends TestCase {
	
	private TestHaskellProject fRightHandle;
	private TestHaskellProject fWrongHandle;

	@Override
	protected void setUp() throws CoreException {
		fRightHandle = new TestHaskellProject("fibb");
		fWrongHandle = new TestHaskellProject("qsort");
	}

	@Override
	protected void tearDown() throws Exception {
		fRightHandle.destroy();
		fWrongHandle.destroy();
	}

	public void testFetchesCorrectLanguageModel() throws CoreException {
		IFile rightFile = fRightHandle.createSourceFile("Fibonacci.hs", "module Fibonacci where\n\n" +
				"fibb 0 = 1\n" +
				"fibb 1 = 1\n");
		IFile wrongFile = fWrongHandle.createSourceFile("QuickSort.hs", "module QuickSort where\n\n" +
				"qsort [] = []");
		
		createViewerFor(wrongFile);
		final TextViewer rightViewer = createViewerFor(rightFile);
		
		final IHaskellModel model = new HaskellLanguageModel();
		
		IHaskellModelManager manager = createMock(IHaskellModelManager.class);
		expect(manager.getModelFor(fRightHandle.getPlatformProject()))
		.andReturn(model);
		replay(manager);
		
		HaskellCompletionContext context = new WorkbenchHaskellCompletionContext(manager, rightViewer, 0);
		
		verify(manager);
		
		assertSame(model, context.getLanguageModel());
	}
	
	private TextViewer createViewerFor(IFile f) throws PartInitException {
		FileEditorInput input = new FileEditorInput(f);
		IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
		ITextEditor editor = (ITextEditor) activePage.openEditor(input, "net.sf.eclipsefp.haskell.ui.editor.HaskellEditor");
		IDocument doc = editor.getDocumentProvider().getDocument(input);
		
		TextViewer viewer = new TextViewer(new Shell(), SWT.NONE);
		viewer.setDocument(doc);
		return viewer;
	}
	
}
