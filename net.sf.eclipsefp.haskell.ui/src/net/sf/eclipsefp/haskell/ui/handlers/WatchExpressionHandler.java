/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.handlers;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor;
import net.sf.eclipsefp.haskell.ui.util.text.WordFinder;
import net.sf.eclipsefp.haskell.ui.util.text.WordFinder.EditorThing;
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugElement;
import org.eclipse.debug.core.model.IWatchExpression;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.handlers.HandlerUtil;


/**
 * Handler for watch expression
 * @author JP Moresmau
 *
 */
public class WatchExpressionHandler extends AbstractHandler {

  /* (non-Javadoc)
   * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event )  {
    IEditorPart editor = HandlerUtil.getActiveEditor( event );

    if( !( editor instanceof HaskellEditor ) ) {
      return null;
    }
    final HaskellEditor hEditor=(HaskellEditor)editor;
    // cast is safe because we have a guard in plugin.xml
    final ITextSelection sel=(ITextSelection)HandlerUtil.getActiveMenuSelection( event );
    String s=sel.getText();
    // find word at location
    if (s.length()==0){
      WordFinder.getEditorThing( hEditor,new WordFinder.EditorThingHandler() {

        @Override
        public void handle( final EditorThing thing ) {
          if (thing!=null && thing.getThing()!=null){
            String name = thing.getThing().getName();
            addExpression( name );
          } else {
            String s=WordFinder.findWord( hEditor.getDocument(), sel.getOffset() );
            if (s.length()>0){
              addExpression( s );
            }
          }
        }
      });
    } else {
      addExpression( s );
    }

    return null;
  }

  protected void addExpression(final String s){
    if (s.length()>0){
      IWatchExpression expression= DebugPlugin.getDefault().getExpressionManager().newWatchExpression(s);
      DebugPlugin.getDefault().getExpressionManager().addExpression(expression);
      IAdaptable object = DebugUITools.getDebugContext();
      IDebugElement context= null;
      if (object instanceof IDebugElement) {
        context= (IDebugElement) object;
      } else if (object instanceof ILaunch) {
        context= ((ILaunch) object).getDebugTarget();
      }
      expression.setExpressionContext(context);
    }
  }
}
