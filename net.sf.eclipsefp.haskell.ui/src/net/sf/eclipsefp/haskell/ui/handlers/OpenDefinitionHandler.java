package net.sf.eclipsefp.haskell.ui.handlers;

import net.sf.eclipsefp.haskell.scion.client.Scion;
import net.sf.eclipsefp.haskell.scion.commands.NameDefinitionsCommand;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;

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
					// TODO open other editor, if definition is in another file
					IDocument document = haskellEditor.getDocument();
					int start;
					try {
						start = document.getLineOffset(command.getStartLine() - 1) + command.getStartColumn();
						int length = document.getLineOffset(command.getEndLine() - 1) + command.getEndColumn() - start;
						haskellEditor.selectAndReveal(start, length);
					} catch (BadLocationException ex) {
						// ignore
					}
				}
			}
		}
		return null;
	}

}
