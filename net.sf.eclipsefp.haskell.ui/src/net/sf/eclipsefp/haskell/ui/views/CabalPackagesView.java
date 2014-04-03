/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.views;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import net.sf.eclipsefp.haskell.browser.BrowserPerspective;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.views.packages.PackagesView;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageRef;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersion;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.handlers.OpenDefinitionHandler;
import net.sf.eclipsefp.haskell.ui.internal.scion.CabalPackageHelper;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.properties.ImportLibrariesLabelProvider;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import net.sf.eclipsefp.haskell.util.CommandLineUtil;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.part.ViewPart;

/**
 * A view showing all installed packages, all packages retrieved from hackage, and buttons to install a package and update the list
 * @author JP Moresmau
 *
 */
public class CabalPackagesView extends ViewPart {
  public static final String ID="net.sf.eclipsefp.haskell.ui.views.CabalPackagesView";

  public static void refresh(){
    /** clear cache **/
    CabalPackageHelper.getInstance().clear();
    Display.getDefault().asyncExec( new Runnable(){
      @Override
      public void run() {

        IWorkbenchPage p=HaskellUIPlugin.getActivePage();
        IViewPart part=p.findView( ID );
        if (part instanceof CabalPackagesView){
          CabalPackagesView v=((CabalPackagesView)part);
          /** only if we show the installed packages **/
          /** nope, now we show ticks for installed packages on list of all packages */
          //if (v.onlyInstalled){
            v.refreshJob.schedule();
          //}
        }
      }
    });
  }

  private CabalPackageHelper helper;
  private Button bAll;

  private boolean onlyInstalled=true;

  private TreeViewer packageViewer;

  private final ImportLibrariesLabelProvider labelProvider=new ImportLibrariesLabelProvider();

  private Label lUpdate;

  /**
   * current selection name (either package or package + version)
   */
  private String currentName;
  /**
   * current selected version (if a package is selected, it's its last known version)
   */
  private String currentNameWithVersion;
  private Label lInstall;
  boolean installed=false;
  boolean installPossible=false;
  private Text infoViewer;

  private Label lSelected;

  private Action updateAction;
  private Action installAction;

  private final Job refreshJob=new Job(UITexts.cabalPackagesView_list_running){
    @Override
    protected IStatus run( final IProgressMonitor arg0 ) {
      try {
        labelProvider.setShowInstalled(!onlyInstalled );
        final List<CabalPackageRef> l=onlyInstalled?helper.getInstalled():helper.getAll();
        CabalPackagesView.this.getSite().getShell().getDisplay().asyncExec( new Runnable(){
          @Override
          public void run() {
            packageViewer.setInput( l );
          }
        } ) ;

      } catch (IOException ioe){
        return new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), UITexts.cabalPackagesView_list_error,ioe );
      }
      return Status.OK_STATUS;
    }
  };

  @Override
  public void createPartControl( final Composite parent ) {
    parent.setLayout( new GridLayout(2,true) );

    Composite buttons=new Composite(parent,SWT.NONE);
    buttons.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    buttons.setLayout( new GridLayout(2,true) );
    final Button bInstalled=new Button( buttons, SWT.RADIO );
    bInstalled.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    bInstalled.setText( UITexts.cabalPackagesView_installed );
    bInstalled.setSelection( true );
    bInstalled.setBackground( buttons.getBackground() );

    bAll=new Button( buttons, SWT.RADIO );
    bAll.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    bAll.setText( UITexts.cabalPackagesView_all );
    bAll.setBackground( buttons.getBackground() );

    lSelected=new Label(parent,SWT.NONE);
    GridData gd=new GridData(GridData.FILL_HORIZONTAL);
    lSelected.setLayoutData(gd);
    lSelected.setText( NLS.bind( UITexts.cabalPackagesView_selected, UITexts.none ) );

    Label lFilter=new Label(parent,SWT.NONE);
    gd=new GridData(GridData.FILL_HORIZONTAL);
    lFilter.setLayoutData(gd);
    lFilter.setText( UITexts.cabalPackagesView_filter );

//    Label lOptions=new Label(parent,SWT.NONE);
//    gd=new GridData(GridData.FILL_HORIZONTAL);
//    lOptions.setLayoutData(gd);
//    lOptions.setText( UITexts.cabalPackagesView_action_install_options );
    new Label(parent,SWT.NONE);

    final Text tFilter=new Text(parent,SWT.BORDER | SWT.SEARCH | SWT.ICON_SEARCH);
    tFilter.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );

    new Label(parent,SWT.NONE);
//    final Text tOptions=new Text(parent,SWT.BORDER);
//    tOptions.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );

    final Label lMatching=new Label(parent,SWT.NONE);
    lMatching.setLayoutData( new GridData(GridData.FILL_HORIZONTAL | GridData.VERTICAL_ALIGN_END) );
    lMatching.setText( UITexts.cabalPackagesView_matching );

    IToolBarManager tbm=getViewSite().getActionBars().getToolBarManager();
    updateAction=new Action(UITexts.cabalPackagesView_action_update,HaskellUIImages.getImageDescriptor( IImageNames.HACKAGE_UPDATE )){
        @Override
        public void run() {
          update();
        }
    };
    tbm.add( updateAction );

    installAction=new Action(UITexts.cabalPackagesView_action_install,HaskellUIImages.getImageDescriptor( IImageNames.HACKAGE_INSTALL )){
      @Override
      public void run() {
        new InstallDialog( getSite().getShell() ).open();
      }
    };
    tbm.add(installAction);
    installAction.setEnabled( false );

//    new Label(parent,SWT.NONE);
//    Composite cInstallButtons=new Composite(parent,SWT.NONE);
//    cInstallButtons.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
//    cInstallButtons.setLayout( new GridLayout(3,false) );
//
//    final Button bUser=new Button(cInstallButtons,SWT.PUSH);
//    bUser.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_CENTER) );
//    bUser.setText(UITexts.cabalPackagesView_action_install_user);
//    bUser.setEnabled( false );
//
//    final Button bGlobal=new Button(cInstallButtons,SWT.PUSH);
//    bGlobal.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_CENTER) );
//    bGlobal.setText(UITexts.cabalPackagesView_action_install_global);
//    bGlobal.setEnabled( false );
//
    lInstall=new Label(parent,SWT.NONE);
    GridData gdInstall=new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
    gdInstall.grabExcessHorizontalSpace=true;
    lInstall.setLayoutData( gdInstall );

    packageViewer=new TreeViewer(parent,SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.SINGLE);
    packageViewer.getTree().setLayoutData( new GridData(GridData.FILL_BOTH) );
    packageViewer.setContentProvider( new CabalPackageContentProvider() );
    packageViewer.setComparator( new CabalPackageViewerComparator() ); // cabal already sorts the data, but takes case into account...
    packageViewer.setLabelProvider(labelProvider );

    packageViewer.addFilter( new ViewerFilter() {

      @Override
      public boolean select( final Viewer viewer, final Object parentElement, final Object element ) {
        if (element instanceof CabalPackageRef){
          String name=((CabalPackageRef)element).getName();
          return name.toLowerCase( Locale.ENGLISH ).startsWith( tFilter.getText().toLowerCase(Locale.ENGLISH) );
        }
        return true;
      }
    });

    Menu m=new Menu( packageViewer.getTree() );
    final Image img=HaskellUIImages.getImageDescriptor( IImageNames.HACKAGE_INSTALL ).createImage() ;

    final MenuItem miInstallShort=new MenuItem( m, SWT.PUSH );
    miInstallShort.setText( UITexts.cabalPackagesView_action_install_short );
    miInstallShort.setImage(img );
    miInstallShort.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent arg0) {
        install( false, "" );
      }
    } );

    final MenuItem miInstallLong=new MenuItem( m, SWT.PUSH );
    miInstallLong.setText( UITexts.cabalPackagesView_action_install );

    miInstallLong.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent arg0) {
        new InstallDialog( getSite().getShell() ).open();
      }
    } );
    miInstallLong.setImage( img);

    final Image browserImg=HaskellUIImages.getImageDescriptor( IImageNames.HASKELL_MISC ).createImage() ;


    final MenuItem miBrowser=new MenuItem( m, SWT.PUSH );
    miBrowser.setText( UITexts.cabalPackagesView_info_browser );

    miBrowser.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent arg0) {
        try {
          IWorkbenchPage page=getViewSite().getWorkbenchWindow().getWorkbench().showPerspective( BrowserPerspective.class.getName(), getViewSite().getWorkbenchWindow() );
          PackagesView view=(PackagesView)page.showView( PackagesView.ID );
          view.select(currentNameWithVersion);
        } catch (Throwable t){
          HaskellUIPlugin.log( t );
        }
      }
    } );
    miBrowser.setImage( browserImg);

    final MenuItem miMore=new MenuItem( m, SWT.PUSH );
    miMore.setText( UITexts.cabalPackagesView_info_more );

    miMore.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final SelectionEvent arg0) {
        new Job(UITexts.cabalPackagesView_info_more_running){
          @Override
          protected IStatus run( final IProgressMonitor arg0 ) {
            OpenDefinitionHandler.openExternalDefinition( getSite().getPage(), null, currentName, null, null, " " );
            return Status.OK_STATUS;
          }
        }.schedule();
      }
    } );
    miMore.setImage( browserImg);

    m.addMenuListener( new MenuListener() {

      @Override
      public void menuShown( final MenuEvent arg0 ) {
        miInstallShort.setEnabled( currentName!=null && installPossible);
        miInstallLong.setEnabled( currentName!=null && installPossible);
        miBrowser.setEnabled( currentNameWithVersion!=null && installed && BrowserPlugin.getDefault().isAnyDatabaseLoaded());
        miMore.setEnabled( currentName!=null );
      }

      @Override
      public void menuHidden( final MenuEvent arg0 ) {
        // NOOP
      }
    } );
    m.addDisposeListener( new DisposeListener() {

      @Override
      public void widgetDisposed( final DisposeEvent arg0 ) {
        img.dispose();
        browserImg.dispose();
      }
    } );

    packageViewer.getTree().setMenu( m );

    infoViewer=new Text(parent,SWT.MULTI | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
    infoViewer.setLayoutData( new GridData(GridData.FILL_BOTH) );

//    Composite cUpdate=new Composite(parent,SWT.NONE);
//    cUpdate.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
//    cUpdate.setLayout( new GridLayout(2,false) );
//    Button bUpdate=new Button(cUpdate,SWT.PUSH);
//    bUpdate.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING) );
//    bUpdate.setText( UITexts.cabalPackagesView_action_update );

    lUpdate=new Label(parent,SWT.NONE);
    lUpdate.setLayoutData(  new GridData(GridData.FILL_HORIZONTAL) );

    helper=CabalPackageHelper.getInstance();

    refreshJob.schedule();

    bInstalled.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final org.eclipse.swt.events.SelectionEvent e) {
        onlyInstalled=true;
        clearInfo();
        packageViewer.setInput( Collections.emptyList());
        refreshJob.schedule();
      }
    });
    bAll.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final org.eclipse.swt.events.SelectionEvent e) {
        onlyInstalled=false;
        clearInfo();
        packageViewer.setInput( Collections.emptyList());
        refreshJob.schedule();
      }
    });

    tFilter.addModifyListener( new ModifyListener() {

      @Override
      public void modifyText( final ModifyEvent paramModifyEvent ) {
        packageViewer.refresh();
      }
    });



    packageViewer.addSelectionChangedListener( new ISelectionChangedListener() {

      @Override
      public void selectionChanged( final SelectionChangedEvent arg0 ) {
        IStructuredSelection sel=(IStructuredSelection)arg0.getSelection();
        installPossible=sel.size()==1 && bAll.getSelection(); // ignore for now the fact that all show also what's installed...
        //bGlobal.setEnabled( installPossible );
        //bUser.setEnabled( installPossible );
        installAction.setEnabled( installPossible );
        if (sel.size()==1){
          currentName=null;
          currentNameWithVersion=null;
          Object o=sel.getFirstElement();
          installed=false;
          if (o instanceof CabalPackageRef){
            currentName=((CabalPackageRef)o).getName();
            for (CabalPackageVersion v:((CabalPackageRef)o).getCabalPackageVersions()){
              if (v.isLast()){
                currentNameWithVersion=currentName+"-"+v.toString();
                installed=v.isInstalled();
                break;
              }
            }
          } else if (o instanceof CabalPackageVersion){
            CabalPackageVersion v=(CabalPackageVersion)o;
            installed=v.isInstalled();
            currentName=v.getRef().getName()+"-"+v.toString();
            //if (v.isLast()){
              currentNameWithVersion=currentName;
            //}
          }
          if (currentName!=null){
            showInfo();
          }
        }

      }
    });



  }

  /**
   * clear all package info
   */
  private void clearInfo(){
    lInstall.setText( "" );
    lSelected.setText( NLS.bind( UITexts.cabalPackagesView_selected, UITexts.none ) );
    infoViewer.setText("");
  }

  /**
   * show info for a given package
   */
  private void showInfo(){
    lSelected.setText( NLS.bind( UITexts.cabalPackagesView_selected, currentName ) );
    if (bAll.getSelection()){
      lInstall.setText( "" );
    } else {
      lInstall.setText( UITexts.cabalPackagesView_info_installed );
    }
    lInstall.getParent().layout( true );
    new Job(UITexts.cabalPackagesView_info_running){
      @Override
      protected IStatus run( final IProgressMonitor arg0 ) {
        try {
          final String s=helper.getInfo( currentName );
          CabalPackagesView.this.getSite().getShell().getDisplay().asyncExec( new Runnable(){
            @Override
            public void run() {
              infoViewer.setText(s);
            }
          } ) ;

        } catch (IOException ioe){
          return new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), UITexts.cabalPackagesView_info_error,ioe );
        }
        return Status.OK_STATUS;
      }
    }.schedule();
  }

  /**
   * install a package
   * @param global (true) or user (false)
   * @param options additional options
   */
  private void install(final boolean global,final String options){
    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    if (cabalExecutable!=null){
      final List<String> commands = new ArrayList<String>();
      commands.add( cabalExecutable );
      commands.add("install");
      commands.add(currentName);
   // options
      if (global){
        commands.add( "--global" );
      } else {
        commands.add( "--user" );
      }
      ScionManager.addCabalInstallOptions( commands );
      // force reinstall
      commands.add( "--reinstall" );
      if (options!=null && options.trim().length()>0){
        commands.addAll(Arrays.asList(CommandLineUtil.parse( options.trim() )));
      }

        try {
          // this requires to be in the UI thread
          AbstractHaskellLaunchDelegate.runInConsole(null, commands, new File(cabalExecutable).getParentFile(), UITexts.cabalPackagesView_action_install_running,true, new Runnable() {

            @Override
            public void run() {
              lInstall.getDisplay().asyncExec( new Runnable() {

                @Override
                public void run() {
                  lInstall.setText( UITexts.cabalPackagesView_info_installed );
                  helper.setInstalled( null );
                  if (!bAll.getSelection()){
                    refreshJob.schedule();
                  }
                }
              });
              BrowserPlugin.loadLocalDatabase( true );
            }
          });
        } catch (Exception ioe){
          HaskellUIPlugin.log(ioe);
          final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);
          ErrorDialog.openError(getSite().getShell(), UITexts.cabalPackagesView_action_install_error, UITexts.cabalPackagesView_action_install_error, st);
        }

    }
  }

  private void update(){
    lUpdate.setText( UITexts.cabalPackagesView_action_update_running );
    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    if (cabalExecutable!=null){
      final List<String> commands = new ArrayList<String>();
      commands.add( cabalExecutable );
      commands.add("update");
      try {
        AbstractHaskellLaunchDelegate.runInConsole(null, commands, new File(cabalExecutable).getParentFile(), UITexts.cabalPackagesView_action_update_running,true, new Runnable() {

          @Override
          public void run() {
            lUpdate.getDisplay().asyncExec( new Runnable() {

              @Override
              public void run() {
                lUpdate.setText( UITexts.cabalPackagesView_action_update_ok );
                helper.setAll( null );
                if (bAll.getSelection()){
                  refreshJob.schedule();
                }
              }
            });

          }
        });
      } catch (Exception ioe){
        HaskellUIPlugin.log(ioe);
        lUpdate.setText( UITexts.cabalPackagesView_action_update_running );
      }
     }

  }


  @Override
  public void setFocus() {
    // TODO Auto-generated method stub

  }

  /**
   * simple dialog to ask global/user and additional options
   * @author JP Moresmau
   *
   */
  private class InstallDialog extends Dialog {
    private boolean global=false;
    private String options="";

    public InstallDialog( final Shell parentShell ) {
      super( parentShell );

    }

    @Override
    protected int getShellStyle() {
      return super.getShellStyle() | SWT.RESIZE;
    }

    @Override
    protected void configureShell( final Shell newShell ) {
      super.configureShell( newShell );
      newShell.setText( UITexts.cabalPackagesView_action_install_options );
    }

    @Override
    protected Control createDialogArea( final Composite parent1 ) {
      Composite parent2=(Composite)super.createDialogArea( parent1 );

      parent2.setLayout( new GridLayout(2,true) );

      final Button bUser=new Button(parent2,SWT.RADIO);
      bUser.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING) );
      bUser.setText(NLS.bind(UITexts.cabalPackagesView_action_install_user,currentName));
      bUser.setSelection( true );

      final Button bGlobal=new Button(parent2,SWT.RADIO);
      bGlobal.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING) );
      bGlobal.setText(NLS.bind(UITexts.cabalPackagesView_action_install_global,currentName));
      bGlobal.setSelection( false );

      Label lOptions=new Label(parent2,SWT.NONE);
      GridData gd=new GridData(GridData.FILL_HORIZONTAL);
      gd.horizontalSpan=2;
      lOptions.setLayoutData(gd);
      lOptions.setText( UITexts.cabalPackagesView_action_install_options );

      final Text tOptions=new Text(parent2,SWT.BORDER);
      gd= new GridData(GridData.FILL_HORIZONTAL);
      gd.horizontalSpan=2;
      tOptions.setLayoutData(gd);

      bUser.addSelectionListener( new SelectionAdapter() {
        @Override
        public void widgetSelected( final SelectionEvent e ) {
          global=false;
        }
      });


      bGlobal.addSelectionListener( new SelectionAdapter() {
        @Override
        public void widgetSelected( final SelectionEvent e ) {
          global=true;
        }
      });

      tOptions.addModifyListener( new ModifyListener() {

        @Override
        public void modifyText( final ModifyEvent arg0 ) {
         options=tOptions.getText();

        }
      });

      return parent2;
    }

    @Override
    protected void okPressed() {
      // install does things in the UI thread, so close the dialog first
      super.okPressed();

      install(global,options);

    }
  }
}
