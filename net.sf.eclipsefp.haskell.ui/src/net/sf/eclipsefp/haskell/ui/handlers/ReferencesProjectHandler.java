/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.handlers;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;


/**
 * @author JP Moresmau
 *
 */
public class ReferencesProjectHandler extends ReferencesWorkspaceHandler {

  /* (non-Javadoc)
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) {
    IEditorPart editor = HandlerUtil.getActiveEditor( event );
    if( !( editor instanceof HaskellEditor ) ) {
      return null;
    }

    final HaskellEditor haskellEditor = ( HaskellEditor )editor;
    project=haskellEditor.findFile().getProject();
    return super.execute( event );
  }

}
