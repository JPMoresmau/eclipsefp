/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.repl;

import net.sf.eclipsefp.haskell.debug.core.internal.HaskellDebugCore;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.internal.ui.views.console.ProcessConsole;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsolePageParticipant;
import org.eclipse.ui.console.TextConsolePage;
import org.eclipse.ui.part.IPageBookViewPage;


/**
 * This console page participant allows us to implement command history
 * @author JP Moresmau
 *
 */
public class HistoryParticipant implements IConsolePageParticipant {

  private HistoryAction action;

  /* (non-Javadoc)
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class arg0 ) {
    return null;
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.console.IConsolePageParticipant#init(org.eclipse.ui.part.IPageBookViewPage, org.eclipse.ui.console.IConsole)
   */
  @Override
  public void init( final IPageBookViewPage paramIPageBookViewPage,
      final IConsole paramIConsole ) {
   TextConsolePage p=(TextConsolePage)paramIPageBookViewPage;

   if (paramIConsole instanceof ProcessConsole){
     IProcess pr=((ProcessConsole)paramIConsole).getProcess();
     if (pr!=null && Boolean.TRUE.toString().equals( pr.getAttribute( HaskellDebugCore.PROCESS_COMMAND_HISTORY ) )){
       action=new HistoryAction(p);

       IActionBars actionBars = paramIPageBookViewPage.getSite().getActionBars();
       actionBars.getToolBarManager().add( action );
     }
   }
  }



  /* (non-Javadoc)
   * @see org.eclipse.ui.console.IConsolePageParticipant#dispose()
   */
  @Override
  public void dispose() {
    if (action!=null){
      action.dispose();
      action=null;
    }
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.console.IConsolePageParticipant#activated()
   */
  @Override
  public void activated() {
    // TODO Auto-generated method stub

  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.console.IConsolePageParticipant#deactivated()
   */
  @Override
  public void deactivated() {
    // TODO Auto-generated method stub

  }

}
