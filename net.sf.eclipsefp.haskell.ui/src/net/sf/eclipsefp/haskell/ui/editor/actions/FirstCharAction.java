//Borrowed code from PyDev by Fabio Zadrozny
package net.sf.eclipsefp.haskell.ui.editor.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;

/**
 * Action to move to the first character on the line or to the line start
 * depending on the cursor position.
 * 
 * @author Fabio Zadrozny
 * @author Andriy Palamarchuk
 * 
 * @author Leif Frenzel (changed formatting and minor refactorings)
 */
public class FirstCharAction extends AbstractAction {
    public void run(final IAction action) {
        final IDocument document =
            getTextEditor().getDocumentProvider().getDocument(getTextEditor().getEditorInput());
        final ITextSelection selection =
            (ITextSelection) getTextEditor().getSelectionProvider().getSelection();

        boolean isAtFirstChar = isAtFirstVisibleChar(document, selection.getOffset());
        if (!isAtFirstChar) {
            gotoFirstVisibleChar(document, selection.getOffset());
        } else {
            gotoFirstChar(document, selection.getOffset());
        }
    }

    @Override
    public void selectionChanged(final IAction action, final ISelection selection) {
        //does nothing
    }

    /**
     * Goes to first character of the line.
     * 
     * @param doc
     * @param cursorOffset
     */
    protected void gotoFirstChar(final IDocument doc, final int cursorOffset) {
        try {
            IRegion region = doc.getLineInformationOfOffset(cursorOffset);
            int offset = region.getOffset();
            setCaretPosition(offset);
        } catch (BadLocationException e) {
            beep(e);
        }
    }

    /**
     * Goes to the first visible char.
     * 
     * @param doc
     * @param cursorOffset
     */
    protected void gotoFirstVisibleChar(final IDocument doc, final int cursorOffset) {
        try {
            setCaretPosition(getFirstCharPosition(doc, cursorOffset));
        } catch (BadLocationException e) {
            beep(e);
        }
    }

    /**
     * Helper for setting caret
     * 
     * @param pos
     * @throws BadLocationException
     */
    protected void setCaretPosition( final int pos ) {
        getTextEditor().selectAndReveal(pos, 0);
    }

    /**
     * Goes to the first visible char.
     * 
     * @param doc
     * @param cursorOffset
     */
    protected boolean isAtFirstVisibleChar(final IDocument doc, final int cursorOffset) {
        try {
            return getFirstCharPosition(doc, cursorOffset) == cursorOffset;
        } catch (BadLocationException e) {
            return false;
        }
    }

    /**
     * Returns the position of the first non whitespace char in the current
     * line.
     * 
     * @param doc
     * @param cursorOffset
     * @return position of the first character of the line (returned as an
     *         absolute offset)
     * @throws BadLocationException
     */
    private static int getFirstCharPosition(final IDocument doc, final int cursorOffset)
            throws BadLocationException {
        IRegion region;
        region = doc.getLineInformationOfOffset(cursorOffset);
        int offset = region.getOffset();
        return offset + getFirstCharRelativePosition(doc, cursorOffset);
    }
}