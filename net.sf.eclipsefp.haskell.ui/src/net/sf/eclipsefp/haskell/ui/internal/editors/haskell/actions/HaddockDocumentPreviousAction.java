package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions;

import net.sf.eclipsefp.haskell.ui.actions.HaskellActionConstants;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;

/** Implements Haddock -> Document Previous item/element */
final class HaddockDocumentPreviousAction extends Action {
  /** The Haddock previous item documentation marker */
  private static final String PREVDOC_MARKER = " -- ^ ";
  /** The documentation for the user to replace */
  static final String USER_REPLACES = UITexts.HaddockDocumentation_user_replaces;

  /** The associated editor */
  private final HaskellEditor editor;

  /** Default constructor */
  HaddockDocumentPreviousAction(final HaskellEditor editor) {
    super(UITexts.HaddockDocumentation_previous_item);
    this.editor = editor;
    setId( HaskellActionConstants.HADDOCK_PREVIOUS );
  }

  @Override
  public void run() {
    // Go to the end of the line in the editor's document, add the "-- ^ " code:
    IDocument doc = editor.getDocument();

    ISelectionProvider selectionProvider = editor.getSelectionProvider();
    ISelection selection = selectionProvider.getSelection();
    if (selection instanceof ITextSelection) {
      ITextSelection textSelection = ( ITextSelection )selection;
      int offset = textSelection.getOffset();

      if (offset > -1) {
        try {
          IRegion currentLine = doc.getLineInformationOfOffset( offset );
          int endOfLineOffset = currentLine.getOffset() + currentLine.getLength();

          doc.replace(endOfLineOffset, 0, PREVDOC_MARKER.concat(USER_REPLACES));

          ITextSelection newCursor = new TextSelection( doc, endOfLineOffset + PREVDOC_MARKER.length(), USER_REPLACES.length() );
          selectionProvider.setSelection( newCursor );
        } catch( BadLocationException ex ) {
          // Ignore and continue...
        }
      }
    }
  }

  @Override
  public boolean isEnabled() {
    return true;
  }
}