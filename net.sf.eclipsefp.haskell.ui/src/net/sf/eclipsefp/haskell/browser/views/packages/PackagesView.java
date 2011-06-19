package net.sf.eclipsefp.haskell.browser.views.packages;

import java.net.URL;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseLoadedEvent;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.IDatabaseLoadedListener;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.part.ViewPart;

public class PackagesView extends ViewPart implements IDatabaseLoadedListener,
    ISelectionChangedListener, IDoubleClickListener {

  /**
   * The ID of the view as specified by the extension.
   */
  public static final String ID = "net.sf.eclipsefp.haskell.browser.views.packages.PackagesView";

  TreeViewer viewer;
  Browser doc;
  PackagesContentProvider provider;

  @Override
  public void createPartControl( final Composite parent ) {
    SashForm form = new SashForm( parent, SWT.VERTICAL );
    viewer = new TreeViewer( form );
    doc = new Browser( form, SWT.NONE );
    doc.setFont( viewer.getControl().getFont() );
    form.setWeights( new int[] { 75, 25 } );

    // Set label provider and sorter
    viewer.setLabelProvider( new PackagesLabelProvider() );
    viewer.setSorter( new PackagesSorter() );
    // Set initial content provider
    provider = new PackagesContentProvider();
    viewer.setContentProvider( provider );
    viewer.setInput( PackagesRoot.ROOT );
    // Hook for listeners
    BrowserPlugin.getDefault().addDatabaseLoadedListener( this );
    // Hook for changes in selection
    viewer.addPostSelectionChangedListener( this );
    // Hook for double clicking
    viewer.addDoubleClickListener( this );
    // Register as selection provider
    getSite().setSelectionProvider( viewer );
  }

  @Override
  public void setFocus() {
    viewer.getControl().setFocus();
  }

  public void databaseLoaded( final DatabaseLoadedEvent e ) {
    Display.getDefault().asyncExec( new Runnable() {

      public void run() {
        // Use the new provider
        provider.uncache();
        viewer.refresh();
      }
    } );
  }

  public void selectionChanged( final SelectionChangedEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    Object o = selection.getFirstElement();
    if( o == null || o instanceof DatabaseType ) {
      doc.setText( "" );
    } else {
      PackagesItem item = ( PackagesItem )o;
      doc.setText( HtmlUtil.generateDocument( null, item.getPackage().getDoc() ) );
    }
  }

  public void doubleClick( final DoubleClickEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    Object o = selection.getFirstElement();
    if( o == null || o instanceof DatabaseType ) {
      return;
    }

    PackagesItem item = ( PackagesItem )o;
    // Open browser
    try {
      IWorkbenchBrowserSupport browserSupport = this.getSite()
          .getWorkbenchWindow().getWorkbench().getBrowserSupport();
      URL webUrl = new URL( HtmlUtil.generatePackageUrl( item.getPackage().getIdentifier() ) );
      IWebBrowser browser = browserSupport.createBrowser(
          IWorkbenchBrowserSupport.AS_EDITOR
              | IWorkbenchBrowserSupport.LOCATION_BAR, null, "Haskell Browser",
          "Haskell Browser" );
      browser.openURL( webUrl );
    } catch( Throwable ex ) {
      // Do nothing
    }
  }
}