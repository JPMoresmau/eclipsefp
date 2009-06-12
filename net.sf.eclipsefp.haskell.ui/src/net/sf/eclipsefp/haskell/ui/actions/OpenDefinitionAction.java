package net.sf.eclipsefp.haskell.ui.actions;

import net.sf.eclipsefp.haskell.scion.client.Scion;
import net.sf.eclipsefp.haskell.scion.commands.NameDefinitionsCommand;
import net.sf.eclipsefp.haskell.ui.internal.actions.ActionMessages;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;

/**
 * Action that opens an editor and scrolls it to the definition of the currently selected element.
 *
 * @author Thomas ten Cate
 */
public class OpenDefinitionAction extends Action implements ISelectionChangedListener {

	HaskellEditor currentEditor;

	public OpenDefinitionAction() {
		currentEditor = null;
		setText(ActionMessages.openDefinitionAction_label);
		setToolTipText(ActionMessages.openDefinitionAction_tooltip);
		setEnabled(true);
	}

	public OpenDefinitionAction(final HaskellEditor editor) {
		this();
		setCurrentEditor(editor);
	}

	public void setCurrentEditor(final HaskellEditor currentEditor) {
		if (this.currentEditor != null) {
			this.currentEditor.getSelectionProvider().removeSelectionChangedListener(this);
		}
		this.currentEditor = currentEditor;
		if (this.currentEditor != null) {
			this.currentEditor.getSelectionProvider().addSelectionChangedListener(this);
		}
	}

	@Override
	public void run() {
		ISelection selection = currentEditor.getSelectionProvider().getSelection();
		if (selection instanceof TextSelection) {
			TextSelection textSel = (TextSelection)selection;
			String name = textSel.getText().trim(); // TODO make work on 0-length selections too
			NameDefinitionsCommand command = new NameDefinitionsCommand(name);
			Scion.syncRunCommand(command, 500);
			if (command.isSuccessful() && command.isFound()) {
				IDocument document = currentEditor.getDocument();
				int start;
				try {
					start = document.getLineOffset(command.getStartLine() - 1) + command.getStartColumn();
					int length = document.getLineOffset(command.getEndLine() - 1) + command.getEndColumn() - start;
					currentEditor.selectAndReveal(start, length);
				} catch (BadLocationException ex) {
					// ignore
				}
			}
		}
	}

	public void selectionChanged(final SelectionChangedEvent event) {
		// ?
	}

}
