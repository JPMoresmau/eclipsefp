package net.sf.eclipsefp.haskell.ui.views;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageHelper;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageRef;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersion;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.handlers.OpenDefinitionHandler;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.properties.ImportLibrariesLabelProvider;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.part.ViewPart;

/**
 * A view showing all installed packages, all packages retrieved from hackage, and buttons to install a package and update the list
 * @author JP Moresmau
 *
 */
public class CabalPackagesView extends ViewPart {
  private CabalPackageHelper helper;
  private Button bAll;

  private boolean onlyInstalled=true;

  private TreeViewer packageViewer;

  private Label lUpdate;

  private String currentName;

  private Label lInstall;

  private Text infoViewer;

  private Link lMore;

  private final Job refreshJob=new Job(UITexts.cabalPackagesView_list_running){
    @Override
    protected IStatus run( final IProgressMonitor arg0 ) {
      try {
        final List<CabalPackageRef> l=onlyInstalled?helper.getInstalled():helper.getAll();
        CabalPackagesView.this.getSite().getShell().getDisplay().asyncExec( new Runnable(){
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

    final Button bInstalled=new Button( parent, SWT.RADIO );
    bInstalled.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    bInstalled.setText( UITexts.cabalPackagesView_installed );
    bInstalled.setSelection( true );

    bAll=new Button( parent, SWT.RADIO );
    bAll.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    bAll.setText( UITexts.cabalPackagesView_all );

    Label lFilter=new Label(parent,SWT.NONE);
    GridData gd=new GridData(GridData.FILL_HORIZONTAL);
    lFilter.setLayoutData(gd);
    lFilter.setText( UITexts.cabalPackagesView_filter );


    Composite cInstallButtons=new Composite(parent,SWT.NONE);
    cInstallButtons.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );
    cInstallButtons.setLayout( new GridLayout(2,true) );

    final Button bUser=new Button(cInstallButtons,SWT.PUSH);
    bUser.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_CENTER) );
    bUser.setText(UITexts.cabalPackagesView_action_install_user);
    bUser.setEnabled( false );

    final Button bGlobal=new Button(cInstallButtons,SWT.PUSH);
    bGlobal.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_CENTER) );
    bGlobal.setText(UITexts.cabalPackagesView_action_install_global);
    bGlobal.setEnabled( false );

    final Text tFilter=new Text(parent,SWT.BORDER);
    tFilter.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );

    lInstall=new Label(parent,SWT.NONE);
    GridData gdInstall=new GridData(GridData.HORIZONTAL_ALIGN_CENTER);
    gdInstall.horizontalSpan=2;
    lInstall.setLayoutData( gdInstall );

    packageViewer=new TreeViewer(parent,SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.SINGLE);
    packageViewer.getTree().setLayoutData( new GridData(GridData.FILL_BOTH) );
    packageViewer.setContentProvider( new CabalPackageContentProvider() );
    //packageViewer.setComparator( new WorkbenchViewerComparator() ); // cabal already sorts the data
    packageViewer.setLabelProvider( new ImportLibrariesLabelProvider() );

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

    infoViewer=new Text(parent,SWT.MULTI | SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL);
    infoViewer.setLayoutData( new GridData(GridData.FILL_BOTH) );

    lMore=new Link(parent,SWT.NONE);
    GridData gdMore=new GridData(GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_END);
    gdMore.horizontalSpan=2;
    lMore.setLayoutData( gdMore );
    lMore.setText("<a>"+ UITexts.cabalPackagesView_info_more +"</a>");
    lMore.setEnabled( false );

    Button bUpdate=new Button(parent,SWT.PUSH);
    bUpdate.setLayoutData( new GridData(GridData.HORIZONTAL_ALIGN_CENTER) );
    bUpdate.setText( UITexts.cabalPackagesView_action_update );

    lUpdate=new Label(parent,SWT.NONE);
    lUpdate.setLayoutData( new GridData(GridData.FILL_HORIZONTAL) );

    helper=new CabalPackageHelper( CabalImplementationManager.getCabalExecutable() );

    refreshJob.schedule();

    bInstalled.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final org.eclipse.swt.events.SelectionEvent e) {
        onlyInstalled=true;
        refreshJob.schedule();
      }
    });
    bAll.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected(final org.eclipse.swt.events.SelectionEvent e) {
        onlyInstalled=false;
        refreshJob.schedule();
      }
    });

    tFilter.addModifyListener( new ModifyListener() {

      public void modifyText( final ModifyEvent paramModifyEvent ) {
        packageViewer.refresh();
      }
    });

    packageViewer.addSelectionChangedListener( new ISelectionChangedListener() {

      public void selectionChanged( final SelectionChangedEvent arg0 ) {
        IStructuredSelection sel=(IStructuredSelection)arg0.getSelection();
        boolean installPossible=sel.size()==1 && bAll.getSelection(); // ignore for now the fact that all show also what's installed...
        bGlobal.setEnabled( installPossible );
        bUser.setEnabled( installPossible );
        if (sel.size()==1){
          currentName=null;
          Object o=sel.getFirstElement();
          if (o instanceof CabalPackageRef){
            currentName=((CabalPackageRef)o).getName();
          } else if (o instanceof CabalPackageVersion){
            CabalPackageVersion v=(CabalPackageVersion)o;
            currentName=v.getRef().getName()+"-"+v.toString();
          }
          lMore.setEnabled( currentName!=null );
          if (currentName!=null){
            showInfo();
          }
        }

      }
    });

    bUpdate.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        update();
      }
    });

    bGlobal.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
       install( true );
      }
    });
    bUser.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
       install( false );
      }
    });

    lMore.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        new Job(UITexts.cabalPackagesView_info_more_running){
          @Override
          protected IStatus run( final IProgressMonitor arg0 ) {
            OpenDefinitionHandler.openExternalDefinition( getSite().getPage(), null, currentName, null, null, ' ' );
            return Status.OK_STATUS;
          }
        }.schedule();


      }
    });
  }

  private void showInfo(){
    if (bAll.getSelection()){
      lInstall.setText( "" );
    } else {
      lInstall.setText( UITexts.cabalPackagesView_info_installed );
    }
    new Job(UITexts.cabalPackagesView_info_running){
      @Override
      protected IStatus run( final IProgressMonitor arg0 ) {
        try {
          final String s=helper.getInfo( currentName );
          CabalPackagesView.this.getSite().getShell().getDisplay().asyncExec( new Runnable(){
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

  private void install(final boolean global){
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
      // force reinstall
      commands.add( "--reinstall" );
      try {
        AbstractHaskellLaunchDelegate.runInConsole(null, commands, new File(cabalExecutable).getParentFile(), UITexts.cabalPackagesView_action_install_running,true, new Runnable() {

          public void run() {
            lInstall.getDisplay().asyncExec( new Runnable() {

              public void run() {
                lInstall.setText( UITexts.cabalPackagesView_info_installed );
                helper.setInstalled( null );
                if (!bAll.getSelection()){
                  refreshJob.schedule();
                }
              }
            });

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

          public void run() {
            lUpdate.getDisplay().asyncExec( new Runnable() {

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

}
