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
import org.eclipse.jface.text.TextUtilities;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;

/** Implements Haddock -> Document Following item/element */
final class HaddockDocumentFollowingAction extends Action {
  /** The associated editor */
  private final HaskellEditor editor;

  /** The Haddock following item documentation marker */
  private static final String FOLLOWDOC_MARKER = " -- | ";
  /** The documentation for the user to replace */
  static final String USER_REPLACES = UITexts.HaddockDocumentation_user_replaces;

  /** Default constructor */
  HaddockDocumentFollowingAction(final HaskellEditor editor) {
    super(UITexts.HaddockDocumentation_following_item);
    this.editor = editor;
    setId( HaskellActionConstants.HADDOCK_FOLLOWING );
    setActionDefinitionId( HaskellActionConstants.HADDOCK_FOLLOWING_DEF_ID );
  }

  @Override
  public void run() {
    // Insert a new line above the current line, inserting the "-- | " code:
    IDocument doc = editor.getDocument();

    ISelectionProvider selectionProvider = editor.getSelectionProvider();
    ISelection selection = selectionProvider.getSelection();
    if (selection instanceof ITextSelection) {
      ITextSelection textSelection = ( ITextSelection )selection;
      int offset = textSelection.getOffset();

      if (offset > -1) {
        try {
          IRegion currentLine = doc.getLineInformationOfOffset( offset );
          int startOfLineOffset = currentLine.getOffset();

          doc.replace(startOfLineOffset, 0,
                      FOLLOWDOC_MARKER.concat(USER_REPLACES).concat( TextUtilities.getDefaultLineDelimiter( doc ) ));

          ITextSelection newCursor = new TextSelection( doc, startOfLineOffset + FOLLOWDOC_MARKER.length(),
                                                        USER_REPLACES.length() );
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