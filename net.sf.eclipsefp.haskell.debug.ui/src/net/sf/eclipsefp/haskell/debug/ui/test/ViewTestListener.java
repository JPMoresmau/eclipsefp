/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.test;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.debug.core.test.ITestListener;
import net.sf.eclipsefp.haskell.debug.core.test.TestSuite;
import net.sf.eclipsefp.haskell.debug.ui.internal.HaskellDebugUI;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;


/**
 * Listener showing the results in the haskell test result view
 * @author JP Moresmau
 *
 */
public class ViewTestListener implements ITestListener {

  public TestResultView getView(){
    IWorkbenchPage p=HaskellUIPlugin.getActivePage();
    //TestResultView view=(TestResultView)p.findView( HaskellDebugUI.TEST_RESULTS_VIEW_ID );
    //p.activate( view );
    try {
      TestResultView view=(TestResultView)p.showView( HaskellDebugUI.TEST_RESULTS_VIEW_ID );
      return view;
    } catch (PartInitException pie){
      HaskellCorePlugin.log( pie );
    }
    return null;

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.test.ITestListener#start(int, java.util.List)
   */
  @Override
  public void start( final TestSuite ts ) {
    Display.getDefault().asyncExec( new Runnable(){
      @Override
      public void run() {
        TestResultView view=getView();
        if (view!=null){
          view.setInput( ts );
        }
      }
    } );


  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.test.ITestListener#update(int, java.util.List)
   */
  @Override
  public void update( final TestSuite ts ) {
    Display.getDefault().asyncExec( new Runnable(){
      @Override
      public void run() {
        TestResultView view=getView();
        if (view!=null){
          view.setInput( ts );
        }
      }
    } );

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.core.test.ITestListener#end(int, java.util.List)
   */
  @Override
  public void end( final TestSuite ts ) {
    Display.getDefault().asyncExec( new Runnable(){
      @Override
      public void run() {
        TestResultView view=getView();
        if (view!=null){
          view.setInput( ts );
        }
      }
    } );
  }

}
