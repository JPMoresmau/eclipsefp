/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.test;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.debug.core.test.TestResult;
import net.sf.eclipsefp.haskell.debug.core.test.TestSuite;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.handlers.OpenDefinitionHandler;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Sash;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.part.ViewPart;


/**
 * Haskell test results view
 * @author JP Moresmau
 *
 */
public class TestResultView extends ViewPart {

  private TreeViewer testTree;
  private StyledText testText;
  private Text tRuns;
  private Text tErrors;
  private Text tFailures;

  private final List<TestSuite> history=new ArrayList<TestSuite>();
  private int historyIndex=-1;

//  private final List<Resource> resources=new ArrayList<Resource>();

  /* (non-Javadoc)
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent ) {
    parent.setLayout( new GridLayout(6,true) );

    CLabel clRuns=new CLabel( parent, SWT.LEFT );
    clRuns.setText( UITexts.test_view_runs );

    tRuns=new Text(parent,SWT.NONE);
    tRuns.setText( "0" ); //$NON-NLS-1$
    tRuns.setEditable( false );

    CLabel clErrors=new CLabel(  parent, SWT.LEFT );
    clErrors.setText( UITexts.test_view_errors );
    clErrors.setImage( HaskellUIImages.getImage( IImageNames.ERROR_OVERLAY) );

    tErrors=new Text(parent,SWT.NONE);
    tErrors.setText( "0" ); //$NON-NLS-1$
    tErrors.setEditable( false );

    CLabel clFailures=new CLabel(  parent, SWT.LEFT );
    clFailures.setText( UITexts.test_view_failures );
    clFailures.setImage( HaskellUIImages.getImage( IImageNames.FAILURE_OVERLAY) );

    tFailures=new Text(parent,SWT.NONE);
    tFailures.setText( "0" ); //$NON-NLS-1$
    tFailures.setEditable( false );

    final Composite mainComposite=new Composite(parent,SWT.NONE);
    final GridData gd=new GridData(GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL | GridData.FILL_BOTH);
    gd.horizontalSpan=6;
    mainComposite.setLayoutData( gd );
    FormLayout fl=new FormLayout();
    mainComposite.setLayout( fl  );

    testTree=new TreeViewer( mainComposite,SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
    final Sash sash=new Sash(  mainComposite, SWT.HORIZONTAL);

    FormData fd=new FormData();
    fd.top=new FormAttachment( 0);
    fd.left=new FormAttachment(0);
    fd.right=new FormAttachment(100);
    fd.bottom=new FormAttachment(sash,0);
    testTree.getTree().setLayoutData( fd );
    testTree.setContentProvider( new TestResultCP());
    testTree.setLabelProvider( new TestResultLP() );

    CLabel clOutput=new CLabel(  mainComposite, SWT.LEFT );
    clOutput.setText( UITexts.test_view_text);

    final FormData sashData=new FormData();
    sashData.top=new FormAttachment( 50, 0 );
    sashData.left=new FormAttachment(0);
    sashData.right=new FormAttachment(100);
    sash.setLayoutData( sashData );


    fd=new FormData();
    fd.top=new FormAttachment( sash, 0 );
    fd.left=new FormAttachment(0);
    fd.right=new FormAttachment(100);
    clOutput.setLayoutData( fd );

    testText=new StyledText( mainComposite, SWT.H_SCROLL | SWT.V_SCROLL  | SWT.BORDER);

    fd=new FormData();
    fd.top=new FormAttachment( clOutput, 0 );
    fd.left=new FormAttachment(0,0);
    fd.right=new FormAttachment(100);
    fd.bottom=new FormAttachment(100);
    testText.setLayoutData( fd );

    testTree.addSelectionChangedListener( new ISelectionChangedListener() {

      @Override
      public void selectionChanged( final SelectionChangedEvent paramSelectionChangedEvent ) {
        Object o=((IStructuredSelection)paramSelectionChangedEvent.getSelection()).getFirstElement();
        if (o instanceof TestResult){
          String txt=((TestResult)o).getText();
          if (txt==null){
            txt=""; //$NON-NLS-1$
          }
          testText.setText( txt );
        }

      }
    } );

    testTree.addDoubleClickListener( new IDoubleClickListener() {

      @Override
      public void doubleClick( final DoubleClickEvent paramDoubleClickEvent ) {
        Object o=((IStructuredSelection)paramDoubleClickEvent.getSelection()).getFirstElement();
        if (o instanceof TestResult){
          TestResult tr=(TestResult)o;
          if (tr.getLocation()!=null && tr.getProject()!=null){
            try {
              OpenDefinitionHandler.openInEditor( tr.getLocation(), tr.getProject() );
            } catch (Throwable t){
              HaskellCorePlugin.log( t );
            }
          }
        }
      }
    } );

    sash.addListener( SWT.Selection, new Listener()  {

      @Override
      public void handleEvent( final Event event ) {
        Rectangle clientArea = mainComposite.getClientArea();
        Rectangle sashRect = sash.getBounds ();
        int top=clientArea.height  - 70; // bottom minimum
        event.y=Math.max (Math.min (event.y, top), 30); // top minimum
        if (event.y!=sashRect.y){
          sashData.top=new FormAttachment(0,event.y);
          mainComposite.layout();
        }
      }
    });


    IToolBarManager tmgr=getViewSite().getActionBars().getToolBarManager();
    HistoryAction act=new HistoryAction();

    tmgr.add( act );
  }

  public void clear(){
    testTree.setInput( Collections.emptySet() );
    tRuns.setText( String.valueOf( 0 ) );
    tErrors.setText( String.valueOf( 0 ) );
    tFailures.setText( String.valueOf( 0 ) );
    tRuns.getParent().layout( new Control[]{tRuns,tErrors,tFailures} );
  }

  /**
   * set the given test suite as input
   * @param ts the test suite
   * @param add add to history?
   */
  public void setInput(final TestSuite ts,final boolean add){
    Object[] expanded=testTree.getExpandedElements();

    /** avoid flicker by only refreshing if we have the proper input already **/
    boolean needInput=true;
    if (!add){
      Object i=testTree.getInput();
      if (i!=null && i instanceof Collection<?>){
        Collection<?> c=(Collection<?>)i;
        if (c.size()==1 && ts.getRoot()==c.iterator().next() ){
          testTree.refresh();
          needInput=false;
        }
      }
    }
    if (needInput){
      testTree.setInput( Collections.singleton( ts.getRoot() ));
    }

    tRuns.setText( String.valueOf( ts.getRuns() ) );
    tErrors.setText( String.valueOf( ts.getErrors() ) );
    tFailures.setText( String.valueOf( ts.getFailures() ) );
    tRuns.getParent().layout( new Control[]{tRuns,tErrors,tFailures} );
    if (add){
      testText.setText( "" ); //$NON-NLS-1$
      history.add( ts );
      historyIndex=history.size()-1;
      testTree.expandToLevel( 2 );
    } else {
      testTree.setExpandedElements( expanded );
    }
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus() {
    if (testTree!=null && !testTree.getTree().isDisposed()){
      testTree.getTree().setFocus();
    }

  }

  /**
   * dynamic history menu
   * @author JP Moresmau
   *
   */
  private class HistoryMenuCreator  implements IMenuCreator{
    private Menu menu;
    /* (non-Javadoc)
     * @see org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets.Control)
     */
    @Override
    public Menu getMenu( final Control paramControl ) {
      dispose();
      menu=new Menu(paramControl);
      fillMenu(menu);
      return menu;
    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets.Menu)
     */
    @Override
    public Menu getMenu( final Menu paramMenu ) {
      dispose();
      menu=new Menu(paramMenu);
      fillMenu(menu);
      return menu;
    }

    private void fillMenu(final Menu m){
      if(history.size()>0){
        TestResultLP lp=new TestResultLP();
        for (int a=history.size()-1;a>=0;a--){
          final TestSuite ts=history.get( a );
          MenuItem mi=new MenuItem(m,SWT.RADIO);
          mi.setText( lp.getText( ts ) );
          mi.setImage( lp.getImage( ts ) );
          if (historyIndex!=a){
            final int idx=a;

            mi.addSelectionListener( new SelectionAdapter() {
              @Override
              public void widgetSelected(final SelectionEvent e) {
                historyIndex=idx;
                setInput( ts,false );
              }
            } );
          } else {
            mi.setSelection( true );
          }
        }
        new MenuItem(m,SWT.SEPARATOR);
        /** clear all terminated **/
        MenuItem mi=new MenuItem(m,SWT.PUSH);
        mi.setText( UITexts.test_history_clear);
        mi.setImage( HaskellUIImages.getImage( IImageNames.REMOVE_ALL ) );
        mi.addSelectionListener( new SelectionAdapter() {
          @Override
          public void widgetSelected(final SelectionEvent e) {
            int a=0;
            for (Iterator<TestSuite> it=history.iterator();it.hasNext();){
              final TestSuite ts=it.next();
              /** terminated= !PENDING **/
              if (ts.getRoot().isFinished()){
                it.remove();
                /** history index moves too **/
                if (historyIndex>=a){
                  historyIndex--;
                }
              }
            }
            /** defensive **/
            if (historyIndex<0){
              historyIndex=history.size()-1;
            }
            if (historyIndex>=0){
              setInput( history.get( historyIndex ), false );
            } else {
              clear(); /** remove **/
            }
          }
        } );
      } else {
        /** nothing to show **/
        MenuItem mi=new MenuItem(m,SWT.PUSH);
        mi.setText( UITexts.test_history_none);
        mi.setEnabled( false );
      }

    }

    /* (non-Javadoc)
     * @see org.eclipse.jface.action.IMenuCreator#dispose()
     */
    @Override
    public void dispose() {
      if (menu!=null){
        menu.dispose();
      }

    }
  }

  /**
   * simple action for test run history, delegates to menu creator
   * @author JP Moresmau
   *
   */
  private class HistoryAction extends Action {
    private HistoryAction(){
      super(UITexts.test_history, SWT.DROP_DOWN);
      this.setImageDescriptor( HaskellUIImages.getImageDescriptor( IImageNames.HISTORY_LIST ) );
      setMenuCreator( new HistoryMenuCreator() );
    }
  }
}
