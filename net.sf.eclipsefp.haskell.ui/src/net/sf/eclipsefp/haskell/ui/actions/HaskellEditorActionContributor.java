// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.actions;

import org.eclipse.core.runtime.Assert;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.BasicTextEditorActionContributor;
import org.eclipse.ui.texteditor.ITextEditor;


/** Haskell editor global action contributor: this installs things like the "Source" menu and associated
 * global actions.
 *
 * @author B. Scott Michel (scooter.phd@gmail.com)
 */
public class HaskellEditorActionContributor extends BasicTextEditorActionContributor {

  public HaskellEditorActionContributor() {
    super();
  }

  @Override
  public void setActiveEditor(final IEditorPart editorPart) {
    ITextEditor textEditor = null;

    if (editorPart instanceof ITextEditor) {
      textEditor = (ITextEditor) editorPart;
    }

    Assert.isNotNull( textEditor );

    IActionBars actionBars= getActionBars();
    actionBars.setGlobalActionHandler(HaskellActionConstants.COMMENT, getAction(textEditor, "Comment")); //$NON-NLS-1$
    actionBars.setGlobalActionHandler(HaskellActionConstants.UNCOMMENT, getAction(textEditor, "Uncomment")); //$NON-NLS-1$
    actionBars.setGlobalActionHandler(HaskellActionConstants.SHIFT_RIGHT, getAction(textEditor, "ShiftRight")); //$NON-NLS-1$
    actionBars.setGlobalActionHandler(HaskellActionConstants.SHIFT_LEFT, getAction(textEditor, "ShiftLeft")); //$NON-NLS-1$
    actionBars.setGlobalActionHandler(HaskellActionConstants.HADDOCK_FOLLOWING, getAction(textEditor, "Haddock.Following"));
  }
}
