package net.sf.eclipsefp.haskell.ui.properties.viewerpart;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.model.WorkbenchViewerComparator;


public abstract class TablePart extends SharedPart {


  private TableViewer viewer;
  private Point minSize = null;

  public TablePart( final String[] buttonLabels ) {
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

  protected TableViewer createStructuredViewer( final Composite parent,
                                                     final int style ) {
    int extendedStyle = style | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.MULTI;
    TableViewer tableViewer=new TableViewer( parent, extendedStyle );
    tableViewer.addSelectionChangedListener( new ISelectionChangedListener() {
      public void selectionChanged( final SelectionChangedEvent evt ) {
        IStructuredSelection ssel = ( IStructuredSelection )evt.getSelection();
        TablePart.this.selectionChanged( ssel );
      }
    } );
    tableViewer.setComparator( new WorkbenchViewerComparator() );
    return tableViewer;
  }

  @Override
  protected void updateEnabledState() {
    getControl().setEnabled( isEnabled() );
    super.updateEnabledState();
  }

  public TableViewer getTableViewer() {
    return viewer;
  }

  // template methods
  ///////////////////


  protected abstract void selectionChanged( IStructuredSelection selection );


}
