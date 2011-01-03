package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions;

import java.util.ResourceBundle;
import net.sf.eclipsefp.haskell.ui.editor.actions.IEditorActionDefinitionIds;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * A text editor action that inserts a pragma comment ("{-#  #-}")
  *
  * @author B. Scott Michel (bscottm@ieee.org)
 */
public class PragmaCommentAction extends TextEditorAction {
  /** Pragma comment begin */
  private static final String PRAGMA_BEGIN_MARKER = "{-#";
  /** Pragma comment end */
  private static final String PRAGMA_END_MARKER = "#-} ";

  /** The usual constructor */
  public PragmaCommentAction( final ResourceBundle bundle, final String prefix, final ITextEditor editor ) {
    super( bundle, prefix, editor );
    setId( HaskellEditor.COMMENT_PRAGMA_ACTION );
    setActionDefinitionId( IEditorActionDefinitionIds.COMMENT_PRAGMA );
  }

  public PragmaCommentAction( final ResourceBundle bundle, final String prefix, final ITextEditor editor, final int style ) {
    super( bundle, prefix, editor, style );
    setId( HaskellEditor.COMMENT_PRAGMA_ACTION );
    setActionDefinitionId( IEditorActionDefinitionIds.COMMENT_PRAGMA );
  }

  @Override
  public void update() {
    super.update();
    if (!isEnabled()) {
      return;
    }
    if (!canModifyEditor()) {
      setEnabled( false );
      return;
    }
  }

  @Override
  public void run() {
    // Insert a pragma comment
    ITextEditor editor = getTextEditor();
    if (editor instanceof HaskellEditor) {
      HaskellEditor hEditor = (HaskellEditor) editor;
      IDocument doc = hEditor.getDocument();

      ISelectionProvider selectionProvider = editor.getSelectionProvider();
      ISelection selection = selectionProvider.getSelection();
      if (selection instanceof ITextSelection) {
        ITextSelection textSelection = ( ITextSelection )selection;
        int offset = textSelection.getOffset();

        if (offset > -1) {
          try {
            doc.replace(offset, 0,
                        PRAGMA_BEGIN_MARKER.concat("  ").concat( PRAGMA_END_MARKER ));

            ITextSelection newCursor = new TextSelection( doc, offset + PRAGMA_BEGIN_MARKER.length() + 1, 0);
            selectionProvider.setSelection( newCursor );
          } catch( BadLocationException ex ) {
            // Ignore it
          }
        }
      }
    }
  }

  @Override
  public boolean isEnabled() {
    return true;
  }
}
