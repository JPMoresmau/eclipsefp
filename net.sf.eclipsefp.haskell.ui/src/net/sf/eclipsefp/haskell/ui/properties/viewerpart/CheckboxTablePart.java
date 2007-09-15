// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.properties.viewerpart;

import org.eclipse.jface.viewers.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/** <p>TODO</p>
  * 
  * @author Leif Frenzel
  */
public abstract class CheckboxTablePart extends SharedPart {

  private StructuredViewer viewer;
  private Point minSize = null;

  public CheckboxTablePart( final String[] buttonLabels ) {
    super( buttonLabels );
  }

  public Control getControl() {
    return viewer.getControl();
  }

  @Override
  protected void createMainControl( final Composite parent, 
                                    final int style, 
                                    final int span ) {
    viewer = createStructuredViewer( parent, style );
    Control control = viewer.getControl();
    GridData gd = new GridData( GridData.FILL_BOTH );
    gd.horizontalSpan = span;
    control.setLayoutData( gd );
    applyMinimumSize();
  }

  public void setMinimumSize( final int width, final int height ) {
    minSize = new Point( width, height );
    if( viewer != null ) {
      applyMinimumSize();
    }
  }

  private void applyMinimumSize() {
    if( minSize != null ) {
      GridData gd = ( GridData )viewer.getControl().getLayoutData();
      gd.widthHint = minSize.x;
      gd.heightHint = minSize.y;
    }
  }

  protected StructuredViewer createStructuredViewer( final Composite parent, 
                                                     final int style ) {
    int extendedStyle = style | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER;
    CheckboxTableViewer tableViewer 
      = CheckboxTableViewer.newCheckList( parent, extendedStyle );
    tableViewer.addSelectionChangedListener( new ISelectionChangedListener() {
      public void selectionChanged( final SelectionChangedEvent evt ) {
        IStructuredSelection ssel = ( IStructuredSelection )evt.getSelection();
        CheckboxTablePart.this.selectionChanged( ssel );
      }
    } );
    tableViewer.addCheckStateListener( new ICheckStateListener() {
      public void checkStateChanged( final CheckStateChangedEvent event ) {
        Object element = event.getElement();
        elementChecked( element, event.getChecked() );
      }
    } );
    return tableViewer;
  }

  @Override
  protected void updateEnabledState() {
    getControl().setEnabled( isEnabled() );
    super.updateEnabledState();
  }

  public CheckboxTableViewer getTableViewer() {
    return ( CheckboxTableViewer )viewer;
  }

  // template methods
  ///////////////////
  
  protected abstract void elementChecked( Object element, boolean checked );

  protected abstract void selectionChanged( IStructuredSelection selection );
}
