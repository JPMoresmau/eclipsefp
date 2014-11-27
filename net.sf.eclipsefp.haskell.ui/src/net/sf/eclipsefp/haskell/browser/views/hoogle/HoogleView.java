/**
 * (c) 2011, Alejandro Serrano & JP Moresmau
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.browser.views.hoogle;

import java.util.ArrayList;
import java.util.Map;
import net.sf.eclipsefp.haskell.browser.BrowserEvent;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.DatabaseLoadedEvent;
import net.sf.eclipsefp.haskell.browser.DatabaseType;
import net.sf.eclipsefp.haskell.browser.IDatabaseLoadedListener;
import net.sf.eclipsefp.haskell.browser.IHoogleLoadedListener;
import net.sf.eclipsefp.haskell.browser.items.DeclarationType;
import net.sf.eclipsefp.haskell.browser.items.HaskellPackage;
import net.sf.eclipsefp.haskell.browser.items.HoogleResult;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultConstructor;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultDeclaration;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultModule;
import net.sf.eclipsefp.haskell.browser.items.HoogleResultPackage;
import net.sf.eclipsefp.haskell.browser.util.HtmlUtil;
import net.sf.eclipsefp.haskell.browser.views.SpecialRoot;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.handlers.OpenDefinitionHandler;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

/**
 * View part for Hoogle search.
 * @author Alejandro Serrano & JP Moresmau
 *
 */
public class HoogleView extends ViewPart implements SelectionListener,
    ISelectionChangedListener, IDoubleClickListener, IHoogleLoadedListener, IDatabaseLoadedListener, ISchedulingRule {

  /**
   * search for the given text in Hoogle as if it was typed in the view
   * @param text the text to show in the view
   */
  public static void searchHoogle(final String text){
    // remove prefix if qualified
    int ix=text.lastIndexOf( '.' );
    final String txt=(ix>-1)
      ?text.substring( ix+1 )
      :text;

    Display.getDefault().asyncExec( new Runnable() {

      @Override
      public void run() {
        try {
          IWorkbench w=PlatformUI.getWorkbench();
          IViewPart p=null;
          for (IWorkbenchWindow iww : w.getWorkbenchWindows()) {
            for (IWorkbenchPage page : iww.getPages()) {
              p=page.findView( ID );
              if (p!=null){
                iww.setActivePage( page );
                page.activate( p );
                break;
              }
            }
          }
          if (p==null){
            p=w.getActiveWorkbenchWindow().getActivePage().showView( ID );
            final HoogleView hv=(HoogleView)p;
            // will run after the notifications hoogleLoaded
            Display.getDefault().asyncExec( new Runnable() {

              @Override
              public void run() {
                hv.text.setText( txt );
                Event evt=new Event();
                evt.widget=hv.text;
                hv.widgetDefaultSelected(new SelectionEvent( evt ));
              }
            });
            return;
              //PlatformUI.getWorkbench().getViewRegistry().find( ID ).createView();
          }

          final HoogleView hv=(HoogleView)p;

          hv.text.setText( txt );
          Event evt=new Event();
          evt.widget=hv.text;
          hv.widgetDefaultSelected(new SelectionEvent( evt ));


        } catch (CoreException ce){
          HaskellUIPlugin.log( ce );
        }
      }
    });
  }

  /**
   * The ID of the view as specified by the extension.
   */
  public static final String ID = "net.sf.eclipsefp.haskell.browser.views.hoogle.HoogleView";

  Text text;
  TreeViewer viewer;
  Browser doc;
  Button localDb;
  Button hackageDb;

  @Override
  public void createPartControl( final Composite parent ) {
    GridLayout layout = new GridLayout();
    layout.numColumns = 1;
    layout.verticalSpacing = layout.horizontalSpacing = 0;
    layout.marginBottom = layout.marginHeight = layout.marginLeft = layout.marginRight = layout.marginTop = layout.marginWidth = 0;
    parent.setLayout( layout );

    Composite dbSelection = new Composite( parent, SWT.NULL );
    GridLayout innerLayout = new GridLayout();
    innerLayout.numColumns = 3;
    innerLayout.makeColumnsEqualWidth= true;
    GridData innerData = new GridData();
    innerData.horizontalAlignment = SWT.FILL;
    innerData.grabExcessHorizontalSpace = true;
    dbSelection.setLayoutData( innerData );
    dbSelection.setLayout( innerLayout );

    Label searchInLabel = new Label( dbSelection, SWT.NULL );
    searchInLabel.setText( UITexts.browser_hoogleSearchIn );

    BrowserPlugin.getDefault().addDatabaseLoadedListener( this );

    localDb = new Button( dbSelection, SWT.CHECK );
    localDb.setText( UITexts.browser_localDatabase );
    localDb.setSelection( true );
    localDb.setEnabled( BrowserPlugin.getDefault().isLocalDatabaseLoaded() );
    localDb.setBackground( dbSelection.getBackground() );

    hackageDb = new Button( dbSelection, SWT.CHECK );
    hackageDb.setText( UITexts.browser_hackageDatabase );
    hackageDb.setSelection( false );
    hackageDb.setEnabled( BrowserPlugin.getDefault().isHackageDatabaseLoaded() );
    hackageDb.setBackground( dbSelection.getBackground() );

    text = new Text( parent, SWT.SINGLE | SWT.SEARCH | SWT.ICON_SEARCH
        | SWT.ICON_CANCEL );
    GridData textData = new GridData();
    textData.horizontalAlignment = SWT.FILL;
    textData.grabExcessHorizontalSpace = true;
    text.setLayoutData( textData );
    text.addSelectionListener( this );
    text.setEnabled( false );

    SashForm form = new SashForm( parent, SWT.VERTICAL );
    GridData formData = new GridData();
    formData.horizontalAlignment = SWT.FILL;
    formData.verticalAlignment = SWT.FILL;
    formData.grabExcessVerticalSpace = true;
    formData.grabExcessHorizontalSpace = true;
    form.setLayoutData( formData );
    viewer = new TreeViewer( form );
    viewer.setLabelProvider( new HoogleLabelProvider() );
    // Load if needed
    if (BrowserPlugin.getDefault().isHoogleLoaded()) {
      hoogleLoaded( null );
    } else {
      hoogleUnloaded( null );
    }

    doc = new Browser( form, SWT.NONE );
    form.setWeights( new int[] { 70, 30 } );
    // Hook for double clicking
    viewer.addDoubleClickListener( this );
    // Hook for changes in selection
    viewer.addPostSelectionChangedListener( this );
    viewer.setContentProvider( new HoogleContentProvider() );
    // Wait the Hoogle database to be ready
    BrowserPlugin.getDefault().addHoogleLoadedListener( this );
  }

  @Override
  public void hoogleLoaded( final BrowserEvent e ) {
    Display.getDefault().asyncExec( new Runnable() {

      @Override
      public void run() {


        //viewer.setLabelProvider( new HoogleLabelProvider() );
        if (!viewer.getTree().isDisposed()){

          if (!text.isDisposed() && text.getText().length()>0){
            search( text.getText() );
          } else {
            viewer.setInput( null );
          }
          viewer.refresh();
        }
        if (!text.isDisposed()){
          text.setEnabled( true );
        }
      }
    } );
  }

  private void search(final String text){
    viewer.setInput( SpecialRoot.SEARCHING );
    viewer.refresh();
    final HoogleSearchResult r=new HoogleSearchResult( localDb.getSelection(),hackageDb.getSelection() );
    final Display d=Display.getCurrent();
    Job job=new Job(UITexts.browser_hoogleSearching){
      /* (non-Javadoc)
       * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
       */
      @Override
      protected IStatus run( final IProgressMonitor monitor ) {
        r.search( text );
        return Status.OK_STATUS;
      }
    };
    job.addJobChangeListener( new JobChangeAdapter(){
      /* (non-Javadoc)
       * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
       */
      @Override
      public void done( final IJobChangeEvent event ) {
        d.asyncExec( new Runnable(){
          /* (non-Javadoc)
           * @see java.lang.Runnable#run()
           */
          @Override
          public void run() {
            if (r.getResults().isEmpty()){
              viewer.setInput( SpecialRoot.EMPTY );
              viewer.refresh();
            } else {
              viewer.setInput( r );
              viewer.refresh();

              Object first = ((HoogleContentProvider)viewer.getContentProvider()).getFirstElement();
              if( first != null ) {
                viewer.setSelection( new StructuredSelection( first ), true );
                viewer.getControl().setFocus();
              }

            }
          }
        } );

      }
    } );
    job.setRule( this );
    job.setPriority( Job.INTERACTIVE );
    job.schedule();
  }


  @Override
  public void hoogleUnloaded( final BrowserEvent e ) {
    Display.getDefault().asyncExec( new Runnable() {

      @Override
      public void run() {
        if (!text.isDisposed()){
          text.setEnabled( false );
        }
        viewer.setInput( SpecialRoot.NO_DATABASE );
        viewer.refresh();
      }
    } );
  }


  @Override
  public void setFocus() {
    if (text!=null && !text.isDisposed()){
      text.setFocus();
    }
    if (hackageDb!=null && !hackageDb.isDisposed()){
      hackageDb.setBackground( hackageDb.getParent().getBackground() );
    }
    if (localDb!=null && !localDb.isDisposed()){
      localDb.setBackground( localDb.getParent().getBackground() );
    }
  }

  @Override
  public void widgetSelected( final SelectionEvent e ) {
    // Do nothing
  }

  @Override
  public void widgetDefaultSelected( final SelectionEvent e ) {
    if( e.detail == SWT.CANCEL ) {
      viewer.setInput( "" );
      viewer.refresh();
    } else {
      search( text.getText() );
    }
  }

  @Override
  @SuppressWarnings ( "unchecked" )
  public void selectionChanged( final SelectionChangedEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();

    Object o = selection.getFirstElement();
    if( o == null || o instanceof SpecialRoot ) {
      doc.setText( "" );
      return;
    }

    // Try to find element to show
    HoogleResult result = null;
    if( o instanceof HoogleResult ) {
      result = ( HoogleResult )o;
    } else {
      Map.Entry<String, Object> entry = ( Map.Entry<String, Object> )o;
      if( entry.getValue() instanceof HoogleResult ) {
        result = ( HoogleResult )entry.getValue();
      }
    }

    if( result != null ) {
      String text = "";
      switch( result.getType() ) {
        case KEYWORD:
          text = HtmlUtil.generateDocument( "keyword "
              + result.getName(), "" );
          break;
        case PACKAGE:
          HaskellPackage pkg = ( ( HoogleResultPackage )result ).getPackage();
          text = HtmlUtil.generateDocument( "package "
              + pkg.getIdentifier().toString(), pkg.getDoc() );
          break;
        case MODULE:
          HoogleResultModule mod = ( HoogleResultModule )result;
          text = HtmlUtil.generateDocument( "module " + mod.getName(), mod
              .getPackageIdentifiers(), null, false, mod.getModule().getDoc() );
          break;
        case DECLARATION:
          HoogleResultDeclaration decl = ( HoogleResultDeclaration )result;
          text = HtmlUtil.generateDocument( decl.getDeclaration()
              .getCompleteDefinition(), decl.getPackageIdentifiers(), decl
              .getModule(), false, decl.getDeclaration().getDoc() );
          break;
        case CONSTRUCTOR:
          HoogleResultConstructor con = ( HoogleResultConstructor )result;
          text = HtmlUtil.generateDocument( con.getConstructor()
              .getCompleteDefinition(), con.getPackageIdentifiers(), con
              .getModule(), false, con.getDeclaration().getDoc() );
          break;
        case WARNING: // not in tree
          break;
      }

      doc.setText( text );
    } else {
      doc.setText( HtmlUtil
          .generateText( UITexts.browser_definedInSeveralLocations ) );
    }
  }

  @Override
  @SuppressWarnings ( "unchecked" )
  public void doubleClick( final DoubleClickEvent event ) {
    TreeSelection selection = ( TreeSelection )event.getSelection();
    Object o = selection.getFirstElement();
    if( o == null || o instanceof SpecialRoot ) {
      return;
    }

    // Try to find element to show
    HoogleResult result = null;
    if( o instanceof HoogleResult ) {
      result = ( HoogleResult )o;
    } else {
      Map.Entry<String, Object> entry = ( Map.Entry<String, Object> )o;
      if( entry.getValue() instanceof HoogleResult ) {
        result = ( HoogleResult )entry.getValue();
      } else {
        // Show the first one (better than nothing)
        result = ( ( ArrayList<HoogleResult> )entry.getValue() ).get( 0 );
      }
    }
    IWorkbenchPage page=getSite().getPage();

    switch( result.getType() ) {
      case KEYWORD:
        OpenDefinitionHandler.openExternalDefinition( page, null, null,null,result.getName(), null );
        break;
      case PACKAGE:
        HoogleResultPackage pkg = ( HoogleResultPackage )result;
        OpenDefinitionHandler.openExternalDefinition( page, null, pkg.getPackage().getIdentifier().toString(),null,null, null );
        break;
      case MODULE:
        HoogleResultModule mod = ( HoogleResultModule )result;
        OpenDefinitionHandler.openExternalDefinition( page, null, mod.getPackageIdentifiers().get( 0 ).toString(),mod.getName(),null, null );
        break;
      case CONSTRUCTOR:
        HoogleResultConstructor con = ( HoogleResultConstructor )result;
        OpenDefinitionHandler.openExternalDefinition( page, null, con.getPackageIdentifiers().get( 0 ).toString(),con.getModule(), con.getName(), "v" );
        break;
      case DECLARATION:
        HoogleResultDeclaration decl = ( HoogleResultDeclaration )result;
        OpenDefinitionHandler.openExternalDefinition( page, null, decl.getPackageIdentifiers().get( 0 ).toString(),decl.getModule(), decl.getName(), decl
          .getDeclaration().getType() == DeclarationType.FUNCTION?"v":"t");
        break;
      case WARNING: // not in tree
        break;
    }
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.browser.IDatabaseLoadedListener#databaseLoaded(net.sf.eclipsefp.haskell.browser.DatabaseLoadedEvent)
   */
  @Override
  public void databaseLoaded( final DatabaseLoadedEvent e ) {
    final Display display = Display.getDefault();
    display.asyncExec( new Runnable() {
      @Override
      public void run() {
        if (e.getType() == DatabaseType.LOCAL) {
          localDb.setEnabled( true );
        } else if (e.getType() == DatabaseType.HACKAGE) {
          hackageDb.setEnabled( true );
        }
      }
    } );

  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.browser.IDatabaseLoadedListener#databaseUnloaded(net.sf.eclipsefp.haskell.browser.BrowserEvent)
   */
  @Override
  public void databaseUnloaded( final BrowserEvent e ) {
    final Display display = Display.getDefault();
    display.asyncExec( new Runnable() {
      @Override
      public void run() {
        localDb.setSelection( false );
        localDb.setEnabled( false );
        hackageDb.setSelection( false );
        hackageDb.setEnabled( false );
      }
    } );
  }

  @Override
  public boolean isConflicting(final ISchedulingRule arg0) {
    return this==arg0;
  }

  @Override
  public boolean contains(final ISchedulingRule arg0) {
    return this==arg0;
  }
}
