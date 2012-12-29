/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.actions;

import java.util.ResourceBundle;
import net.sf.eclipsefp.haskell.ui.editor.actions.IEditorActionDefinitionIds;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.imports.ImportCleaner;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;


/**
 * @author JP Moresmau
 *
 */
public class OrganizeImportAction extends TextEditorAction {

  /**
   * @param bundle
   * @param prefix
   * @param editor
   */
  public OrganizeImportAction( final ResourceBundle bundle, final String prefix,
      final ITextEditor editor ) {
    super( bundle, prefix, editor );
    setId( HaskellEditor.IMPORTS_ACTION );
    setActionDefinitionId( IEditorActionDefinitionIds.IMPORTS );
  }

  /**
   * @param bundle
   * @param prefix
   * @param editor
   * @param style
   */
  public OrganizeImportAction( final ResourceBundle bundle, final String prefix,
      final ITextEditor editor, final int style ) {
    super( bundle, prefix, editor, style );
    setId( HaskellEditor.IMPORTS_ACTION );
    setActionDefinitionId( IEditorActionDefinitionIds.IMPORTS );
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

  /* (non-Javadoc)
   * @see org.eclipse.jface.action.Action#run()
   */
  @Override
  public void run() {
    ITextEditor editor = getTextEditor();
    new ImportCleaner().cleanFile( editor );
  }
}
