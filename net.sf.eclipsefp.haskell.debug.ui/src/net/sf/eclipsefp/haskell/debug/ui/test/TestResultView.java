/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.test;

import java.util.Collections;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.debug.core.test.TestResult;
import net.sf.eclipsefp.haskell.debug.core.test.TestSuite;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.handlers.OpenDefinitionHandler;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.custom.StyledText;
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

  }

  public void setInput(final TestSuite ts){
    testTree.setInput( Collections.singleton( ts.getRoot() ));
    testTree.expandToLevel( 2 );

    tRuns.setText( String.valueOf( ts.getRuns() ) );
    tErrors.setText( String.valueOf( ts.getErrors() ) );
    tFailures.setText( String.valueOf( ts.getFailures() ) );
    tRuns.getParent().layout( new Control[]{tRuns,tErrors,tFailures} );
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

}
