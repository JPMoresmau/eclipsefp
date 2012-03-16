/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.packages;

import java.net.URL;
import net.sf.eclipsefp.haskell.browser.BrowserEvent;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseLoadedEvent;
import net.sf.eclipsefp.haskell.browser.IDatabaseLoadedListener;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import net.sf.eclipsefp.haskell.browser.views.NoDatabaseContentProvider;
import net.sf.eclipsefp.haskell.browser.views.NoDatabaseLabelProvider;
import net.sf.eclipsefp.haskell.browser.views.NoDatabaseRoot;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;
import org.eclipse.ui.part.ViewPart;

/**
 * View part for the 'packages' in the Browser.
 * @author Alejandro Serrano
 *
 */
public class PackagesView extends ViewPart implements IDatabaseLoadedListener,
    ISelectionChangedListener, IDoubleClickListener {

  /**
   * The ID of the view as specified by the extension.
   */
  public static final String ID = "net.sf.eclipsefp.haskell.browser.views.packages.PackagesView";

  TreeViewer viewer;
  Browser doc;
  IContentProvider provider;

  @Override
  public void createPartControl( final Composite parent ) {
    SashForm form = new SashForm( parent, SWT.VERTICAL );
    viewer = new TreeViewer( form );
    doc = new Browser( form, SWT.NONE );
    doc.setFont( viewer.getControl().getFont() );
    form.setWeights( new int[] { 75, 25 } );

    // Set database
    if (BrowserPlugin.getDefault().isAnyDatabaseLoaded()) {
      databaseLoaded( null );
    } else {
      databaseUnloaded( null );
    }
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

  @Override
  public void databaseLoaded( final DatabaseLoadedEvent e ) {
    Display.getDefault().asyncExec( new Runnable() {

      @Override
      public void run() {
        // Set real content provider
        PackagesContentProvider daProvider = new PackagesContentProvider();
        provider = daProvider;
        viewer.setContentProvider( provider );
        viewer.setLabelProvider( new PackagesLabelProvider() );
        viewer.setSorter( new PackagesSorter() );
        // Refresh with the items
        viewer.setInput( PackagesRoot.ROOT );
        // Use the new provider
        daProvider.uncache();
        viewer.refresh();
      }
    } );
  }

  @Override
  public void databaseUnloaded( final BrowserEvent e ) {
    Display.getDefault().asyncExec( new Runnable() {

      @Override
      public void run() {
        if (!BrowserPlugin.getDefault().isAnyDatabaseLoaded()) {
          // Put the "no database" content and label
          viewer.setLabelProvider( new NoDatabaseLabelProvider( false ) );
          viewer.setSorter( new ViewerSorter() );
          provider = new NoDatabaseContentProvider();
          viewer.setContentProvider( provider );
          viewer.setInput( NoDatabaseRoot.ROOT );
          viewer.refresh();
        } else {
          PackagesContentProvider daProvider =
              (PackagesContentProvider)viewer.getContentProvider();
          // Refresh with the items
          viewer.setInput( PackagesRoot.ROOT );
          // Use the new provider
          daProvider.uncache();
          viewer.refresh();
        }
      }
    } );
  }

  @Override
  public void selectionChanged( final SelectionChangedEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    Object o = selection.getFirstElement();
    if( o == null || !( o instanceof PackagesItem ) ) {
      doc.setText( "" );
    } else {
      PackagesItem item = ( PackagesItem )o;
      doc.setText( HtmlUtil.generateDocument( null, item.getPackage().getDoc() ) );
    }
  }

  @Override
  public void doubleClick( final DoubleClickEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    Object o = selection.getFirstElement();
    if( o == null || !( o instanceof PackagesItem ) ) {
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

  public boolean has(final String name) {
    PackagesContentProvider pcp=(PackagesContentProvider)viewer.getContentProvider();
    for (PackagesItem[] items : new PackagesItem[][]{ pcp.getLocalCache(), pcp.getHackageCache() }) {
      for (PackagesItem pi : items){
        if (pi.getPackage().getIdentifier().toString().equals( name )){
          return true;
        }
      }
    }
    return false;
  }

  public void select(final String name){
    PackagesContentProvider pcp=(PackagesContentProvider)viewer.getContentProvider();
    for (PackagesItem[] items : new PackagesItem[][]{ pcp.getLocalCache(), pcp.getHackageCache() }) {
      for (PackagesItem pi : items){
        if (pi.getPackage().getIdentifier().toString().equals( name )){
          viewer.setSelection( new StructuredSelection(pi) );
          return;
        }
      }
    }
  }
}