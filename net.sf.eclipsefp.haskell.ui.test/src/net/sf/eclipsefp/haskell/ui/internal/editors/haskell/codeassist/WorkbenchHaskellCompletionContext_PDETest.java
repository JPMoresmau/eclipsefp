/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais
 *******************************************************************************/
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist;

import net.sf.eclipsefp.haskell.core.internal.util.TestHaskellProject;
import net.sf.eclipsefp.haskell.core.test.TestCaseWithPreferences;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.codeassist.doubles.StubViewer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;

public class WorkbenchHaskellCompletionContext_PDETest extends TestCaseWithPreferences {

	private TestHaskellProject fRightHandle;
	private TestHaskellProject fWrongHandle;

	@Override
	protected void setUp() throws Exception {
	  super.setUp();
		fRightHandle = new TestHaskellProject("fibb", getCorePrefs());
		fWrongHandle = new TestHaskellProject("qsort", getCorePrefs());
	}

	@Override
	protected void tearDown() throws Exception {
		fRightHandle.destroy();
		fWrongHandle.destroy();
		super.tearDown();
	}

	public void testFetchesCorrectLanguageModel() throws CoreException {
		IFile rightFile = fRightHandle.createSourceFile("Fibonacci.hs", "module Fibonacci where\n\n" +
				"fibb 0 = 1\n" +
				"fibb 1 = 1\n");
		IFile wrongFile = fWrongHandle.createSourceFile("QuickSort.hs", "module QuickSort where\n\n" +
				"qsort [] = []");

		createViewerFor(wrongFile);
		final ITextViewer rightViewer = createViewerFor(rightFile);

//		final IHaskellModel model = new HaskellLanguageModel();
//
//		IHaskellModelManager manager = createMock(IHaskellModelManager.class);
//		expect(manager.getModelFor(fRightHandle.getPlatformProject()))
//			.andReturn(model);
//		replay(manager);
//
//		HaskellCompletionContext context = new WorkbenchHaskellCompletionContext(manager, rightViewer, 0);
//
//		verify(manager);
//
//		assertSame(model, context.getLanguageModel());
	}

	public void testParsesCurrentSourceCode() throws CoreException {
		final String oldText = "module Fibonacci where\n\n" +
							   "fibb 0 = 1\n" +
							   "fibb 1 = 1\n";

		IFile fibbFile = fRightHandle.createSourceFile("Fibonacci.hs", oldText);

		final ITextEditor fibbEditor = createEditorFor(fibbFile);
		final IDocument fibbDoc = getDocumentFrom(fibbEditor);
		final ITextViewer fibbViewer = createViewerFor(fibbDoc);

		final String currentText = oldText + "fibb n = (fibb (n - 1)) + (fibb (n - 1))\n";
		fibbDoc.set(currentText);

//		final ICompilationUnit unit = org.easymock.EasyMock.createMock(ICompilationUnit.class);
//
//		IHaskellParser parser = createMock(IHaskellParser.class);
//		expect(parser.parse(fibbFile, currentText))
//			.andReturn(unit);
//		replay(parser);
//
//		HaskellCompletionContext context = new WorkbenchHaskellCompletionContext(parser, fibbViewer, 0);
//
//		fibbEditor.doRevertToSaved();
//
//		verify(parser);
//
//		assertSame(unit, context.getCompilationUnit());
	}

	private ITextViewer createViewerFor(final IFile f) throws PartInitException {
		IDocument doc = createEditorDocumentFor(f);
		return createViewerFor(doc);
	}

	private ITextViewer createViewerFor(final IDocument doc) {
		return new StubViewer(doc);
	}

	private IDocument createEditorDocumentFor(final IFile file) throws PartInitException {
		ITextEditor editor = createEditorFor(file);
		return getDocumentFrom(editor);
	}

	private IDocument getDocumentFrom(final ITextEditor editor) {
		final IEditorInput input = editor.getEditorInput();
		IDocument doc = editor.getDocumentProvider().getDocument(input);
		return doc;
	}

	private ITextEditor createEditorFor(final IFile file) throws PartInitException {
		FileEditorInput input = new FileEditorInput(file);
		IWorkbenchPage activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		return (ITextEditor) activePage.openEditor(input, "net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor");
	}

}
