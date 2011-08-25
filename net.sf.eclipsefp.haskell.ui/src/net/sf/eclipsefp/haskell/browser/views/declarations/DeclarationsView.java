/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.declarations;

import java.net.URL;
import net.sf.eclipsefp.haskell.browser.items.Constructor;
import net.sf.eclipsefp.haskell.browser.items.DeclarationType;
import net.sf.eclipsefp.haskell.browser.items.PackageIdentifier;
import net.sf.eclipsefp.haskell.browser.items.QueryItem;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import net.sf.eclipsefp.haskell.browser.views.modules.ModulesItem;
import net.sf.eclipsefp.haskell.browser.views.packages.PackagesItem;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
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
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.part.ViewPart;

/**
 * Base class for types and functions views, which share the
 * idea of showing a Haskell declaration.
 * @author Alejandro Serrano
 *
 */
public abstract class DeclarationsView extends ViewPart implements
    ISelectionListener, ISelectionChangedListener, IDoubleClickListener {

  /**
   * The ID of the view as specified by the extension.
   */
  public static final String ID = "net.sf.eclipsefp.haskell.browser.views.declarations.DeclarationsView";

  protected boolean isTypes;
  TreeViewer viewer;
  Browser doc;
  DeclarationsContentProvider provider;

  public DeclarationsView( final boolean isTypes ) {
    super();
    this.isTypes = isTypes;
  }

  @Override
  public void createPartControl( final Composite parent ) {
    SashForm form = new SashForm( parent, SWT.VERTICAL );
    viewer = new TreeViewer( form );
    doc = new Browser( form, SWT.NONE );
    form.setWeights( new int[] { 75, 25 } );

    // Set label provider and sorter
    viewer.setLabelProvider( new DeclarationsLabelProvider() );
    viewer.setSorter( new DeclarationsSorter() );
    // Set content provider
    provider = new DeclarationsContentProvider( isTypes );
    viewer.setContentProvider( provider );
    viewer.setInput( null );
    // Hook for changes in selection
    viewer.addPostSelectionChangedListener( this );
    // Hook for double clicking
    viewer.addDoubleClickListener( this );
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

  // Last module loaded, needed for opening help
  ModulesItem lastModulesItem = null;

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
    if( o instanceof PackagesItem ) {
      viewer.setInput( null );
      viewer.refresh();
    }
    if( o instanceof ModulesItem ) {
      this.lastModulesItem = ( ModulesItem )o;
      viewer.setInput( o );
      viewer.refresh();
    }
  }

  public void selectionChanged( final SelectionChangedEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();

    Object o = selection.getFirstElement();
    if( o == null ) {
      doc.setText( "" );
      return;
    }

    if( o instanceof QueryItem ) {
      QueryItem decl = ( QueryItem )o;
      doc.setText( HtmlUtil.generateDocument( decl.getDeclaration()
          .getCompleteDefinition(), decl.getPackages(), null, false, decl
          .getDeclaration().getDoc() ) );
    } else if( o instanceof Constructor ) {
      Constructor c = ( Constructor )o;
      doc.setText( HtmlUtil.generateDocument( c.getCompleteDefinition(),
          c.getDoc() ) );
    }
  }

  public void doubleClick( final DoubleClickEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    Object o = selection.getFirstElement();
    if( o == null || o instanceof Constructor ) {
      return;
    }

    QueryItem item = ( QueryItem )o;
    if( item.getDeclaration().getType() == DeclarationType.INSTANCE ) {
      return; // No documentation for instances
    }
    // Open browser
    try {
      IWorkbenchBrowserSupport browserSupport = this.getSite()
          .getWorkbenchWindow().getWorkbench().getBrowserSupport();
      URL webUrl = new URL( generateUrl( item ) );
      IWebBrowser browser = browserSupport.createBrowser(
          IWorkbenchBrowserSupport.AS_EDITOR
              | IWorkbenchBrowserSupport.LOCATION_BAR, null, "Haskell Browser",
          "Haskell Browser" );
      browser.openURL( webUrl );
    } catch( Throwable ex ) {
      // Do nothing
    }
  }

  public String generateUrl( final QueryItem item ) {
    PackageIdentifier pkg = item.getPackages().get( 0 );
    String moduleName = lastModulesItem.getModule().getName();
    String itemName = item.getName();
    boolean isFunctionLike = (item.getType() == DeclarationType.FUNCTION);

    return HtmlUtil.generateElementUrl( pkg, moduleName, isFunctionLike, itemName );
  }
}
