// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.codeassist;

import net.sf.eclipsefp.haskell.core.codeassist.CompletionEngine;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.contentassist.*;
import org.eclipse.ui.*;
import org.eclipse.ui.texteditor.ITextEditor;

import net.sf.eclipsefp.haskell.core.halamo.ICompilationUnit;
import net.sf.eclipsefp.haskell.core.parser.ParserManager;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;

/**
 * <p>
 * computes the code assist completion proposals and context information.
 * </p>
 * 
 * @author Leif Frenzel
 */
public class HaskellCAProcessor implements IContentAssistProcessor {

	private CompletionEngine fEngine = null;

	public HaskellCAProcessor() {
		// placeholder constructor
	}

	public HaskellCAProcessor(CompletionEngine engine) {
		fEngine = engine;
	}

	// interface methods of IContentAssistProcessor
	// /////////////////////////////////////////////

	public ICompletionProposal[] computeCompletionProposals(
			final ITextViewer viewer, final int offset) {

		try {
			IFile tmp = getFile(viewer);
			if (null != tmp) {
				ICompilationUnit unit = ParserManager.getInstance().getParser()
						.parse(tmp);
				return getCompletionEngine().complete(unit, offset);
			}
			return new ICompletionProposal[0];
		} catch (CoreException ex) {
			HaskellUIPlugin.log("Problem while parsing for proposal.", ex);
		}
		return new ICompletionProposal[0];
	}

	private IFile getFile(final ITextViewer viewer) {
		IDocument currentDocument = viewer.getDocument();

		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		IEditorReference editorReferences[] = window.getActivePage().getEditorReferences();

		IEditorInput input = null;

		for (int i = 0; i < editorReferences.length; i++) {
			IEditorPart editor = editorReferences[i].getEditor(false);
			if (editor instanceof ITextEditor) {
				ITextEditor textEditor = (ITextEditor) editor;
				IDocument doc = textEditor.getDocumentProvider().getDocument(
						input);
				if (!currentDocument.equals(doc)) {
					input = textEditor.getEditorInput();
					break;
				}
			}
		}

		if (input instanceof IFileEditorInput) {
			IFileEditorInput fileInput = (IFileEditorInput) input;
			return fileInput.getFile();
		} else {
			return new HaskellFile(currentDocument.get());
		}

	}

	public IContextInformation[] computeContextInformation(
			final ITextViewer viewer, final int documentOffset) {
		// TODO Auto-generated method stub
		return null;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
		// TODO get from pref and update on pref change
		// return new char[] { '.' };
		return null;
	}

	public char[] getContextInformationAutoActivationCharacters() {
		// TODO Auto-generated method stub
		return null;
	}

	public String getErrorMessage() {
		// return null to indicate we had no problems
		return null;
	}

	public IContextInformationValidator getContextInformationValidator() {
		// TODO Auto-generated method stub
		return null;
	}

	protected CompletionEngine getCompletionEngine() {
		if (fEngine == null) {
			fEngine = new CompletionEngine();
		}
		return fEngine;
	}

}