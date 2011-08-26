/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.modules;

import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import net.sf.eclipsefp.haskell.browser.views.packages.PackagesItem;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;

/**
 * View part for showing the 'modules' in the Browser.
 * @author Alejandro Serrano
 *
 */
public class ModulesView extends ViewPart implements ISelectionListener,
    ISelectionChangedListener {

  /**
   * The ID of the view as specified by the extension.
   */
  public static final String ID = "net.sf.eclipsefp.haskell.browser.views.modules.ModulesView";

  TreeViewer viewer;
  Browser doc;
  ModulesContentProvider provider;

  private IMemento memento;

  @Override
  public void init( final IViewSite site, final IMemento memento )
      throws PartInitException {
    super.init( site, memento );
    this.memento = memento;
  }

  @Override
  public void saveState( final IMemento memento ) {
    super.saveState( memento );
    provider.saveState( memento );
  }

  @Override
  public void createPartControl( final Composite parent ) {
    SashForm form = new SashForm( parent, SWT.VERTICAL );
    viewer = new TreeViewer( form );
    doc = new Browser( form, SWT.NONE );
    form.setWeights( new int[] { 75, 25 } );

    // Set label provider and sorter
    viewer.setLabelProvider( new ModulesLabelProvider() );
    viewer.setSorter( new ModulesSorter() );
    // Set content provider
    provider = new ModulesContentProvider( this.memento );
    viewer.setContentProvider( provider );
    viewer.setInput( null );
    // Hook for changes in selection
    viewer.addPostSelectionChangedListener( this );
    // Register as selection provider
    getSite().setSelectionProvider( viewer );
    // Hook onto selection changes
    getSite().getPage().addPostSelectionListener( this );
  }

  @Override
  public void setFocus() {
    viewer.getControl().setFocus();
  }

  @Override
  public void dispose() {
    getSite().getPage().removePostSelectionListener( this );
    super.dispose();
  }

  // This will be called when a new package is selected
  public void selectionChanged( final IWorkbenchPart part,
      final ISelection selection ) {
    if( part == this ) {
      return;
    }
    if( !( selection instanceof IStructuredSelection ) ) {
      return;
    }
    IStructuredSelection sel = ( IStructuredSelection )selection;
    Object o = sel.getFirstElement();
    if( o == null ) {
      return;
    }
    if( o instanceof DatabaseType || o instanceof PackagesItem ) {
      viewer.setInput( o );
    }
  }

  public void setHierarchical( final boolean isH ) {
    if( provider.getHierarchical() != isH ) {
      provider.setHierarchical( isH );
      viewer.refresh();
    }
  }

  public boolean getHierarchical() {
    return provider.getHierarchical();
  }

  public void selectionChanged( final SelectionChangedEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    ModulesItem item = ( ModulesItem )selection.getFirstElement();
    if( item == null || item.getModule() == null ) {
      doc.setText( "" );
    } else {
      doc.setText( HtmlUtil.generateDocument( null, item.getModule().getDoc() ) );
    }
  }
}
