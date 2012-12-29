// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.actions;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.runtime.Assert;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.BasicTextEditorActionContributor;
import org.eclipse.ui.texteditor.ITextEditor;


/** Haskell editor global action contributor: this installs things like the "Source" menu and associated
 * global actions.
 *
 * @author B. Scott Michel (bscottm@ieee.org)
 */
public class HaskellEditorActionContributor extends BasicTextEditorActionContributor {
  /** The constructor */
  public HaskellEditorActionContributor() {
    super();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void setActiveEditor(final IEditorPart editorPart) {
    ITextEditor textEditor = null;

    if (editorPart instanceof ITextEditor) {
      textEditor = (ITextEditor) editorPart;
    }

    Assert.isNotNull( textEditor );

    IActionBars actionBars= getActionBars();

    // Activate command -> action mappings
    actionBars.setGlobalActionHandler(IHaskellActionConstants.COMMENT,
                                      getAction(textEditor, HaskellEditor.LINE_COMMENT_ACTION));
    actionBars.setGlobalActionHandler(IHaskellActionConstants.UNCOMMENT,
                                      getAction(textEditor, HaskellEditor.LINE_UNCOMMENT_ACTION));
    actionBars.setGlobalActionHandler( IHaskellActionConstants.COMMENT_PRAGMA,
                                      getAction(textEditor, HaskellEditor.COMMENT_PRAGMA_ACTION));
    actionBars.setGlobalActionHandler( IHaskellActionConstants.FORMAT,
        getAction(textEditor, HaskellEditor.FORMAT_ACTION));
    actionBars.setGlobalActionHandler( IHaskellActionConstants.IMPORTS,
        getAction(textEditor, HaskellEditor.IMPORTS_ACTION));

    actionBars.setGlobalActionHandler(IHaskellActionConstants.SHIFT_RIGHT, getAction(textEditor, "ShiftRight")); //$NON-NLS-1$
    actionBars.setGlobalActionHandler(IHaskellActionConstants.SHIFT_LEFT, getAction(textEditor, "ShiftLeft")); //$NON-NLS-1$

    actionBars.setGlobalActionHandler(IHaskellActionConstants.HADDOCK_FOLLOWING,
                                      getAction(textEditor, HaskellEditor.HADDOCK_DOCUMENT_FOLLOWING_ACTION));
    actionBars.setGlobalActionHandler(IHaskellActionConstants.HADDOCK_PREVIOUS,
                                      getAction(textEditor, HaskellEditor.HADDOCK_DOCUMENT_PREVIOUS_ACTION));
    actionBars.setGlobalActionHandler(IHaskellActionConstants.HADDOCK_BLOCK_FOLLOWING,
                                      getAction(textEditor, HaskellEditor.HADDOCK_BLOCK_DOCUMENT_FOLLOWING_ACTION));

    super.setActiveEditor( editorPart );
  }
}
