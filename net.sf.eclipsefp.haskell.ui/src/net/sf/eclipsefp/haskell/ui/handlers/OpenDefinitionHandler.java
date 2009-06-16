package net.sf.eclipsefp.haskell.ui.handlers;

import java.net.URI;
import java.net.URISyntaxException;

import net.sf.eclipsefp.haskell.scion.client.Scion;
import net.sf.eclipsefp.haskell.scion.commands.NameDefinitionsCommand;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * Action that opens an editor and scrolls it to the definition of the currently selected element.
 *
 * @author Thomas ten Cate
 */
public class OpenDefinitionHandler extends AbstractHandler {

	public OpenDefinitionHandler() {
		// explicit default constructor
	}

	public Object execute(final ExecutionEvent event) {
		IEditorPart editor = HandlerUtil.getActiveEditor(event);
		if (editor instanceof HaskellEditor) {
			HaskellEditor haskellEditor = (HaskellEditor)editor;
			ISelection selection = haskellEditor.getSelectionProvider().getSelection();
			if (selection instanceof TextSelection) {
				TextSelection textSel = (TextSelection)selection;
				String name = textSel.getText().trim(); // TODO make work on 0-length selections too
				NameDefinitionsCommand command = new NameDefinitionsCommand(name);
				Scion.syncRunCommand(command, 500);
				if (command.isSuccessful() && command.isFound()) {
					try {
						openInEditor(haskellEditor.getEditorSite().getPage(),
								command.getFileName(),
								command.getStartLine() - 1, command.getStartColumn(),
								command.getEndLine() - 1, command.getEndColumn());
					} catch (PartInitException ex) {
						// too bad
					} catch (URISyntaxException ex) {
						// too bad
					}
				}
			}
		}
		return null;
	}

	protected void openInEditor(final IWorkbenchPage page, final String fileName, final int startLine, final int startColumn, final int endLine, final int endColumn) throws PartInitException, URISyntaxException {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();
		URI uri = new URI("file", "", fileName, null, null);
		IFile[] files = root.findFilesForLocationURI(uri, IResource.FILE);
		if (files.length > 0) {
			IFile file = files[0]; // open only the first file; they should be the same anyway
			IEditorPart editor = IDE.openEditor(page, file, true);
			ITextEditor textEditor = (ITextEditor)editor;
			IDocument document = textEditor.getDocumentProvider().getDocument(editor.getEditorInput());
			int start;
			try {
				start = document.getLineOffset(startLine) + startColumn;
				int length = document.getLineOffset(endLine) + endColumn - start;
				textEditor.selectAndReveal(start, length);
			} catch (BadLocationException ex) {
				// ignore
			}
		}
	}

}
