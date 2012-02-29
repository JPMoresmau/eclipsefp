package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.util.Date;
import java.util.List;
import java.util.Stack;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.browser.Database;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.JobFacade;
import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersion;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.hlint.HLintPlugin;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.console.HaskellConsole;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.CabalFileChangeListener;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.texteditor.IDocumentProvider;

/**
 * Manages helper executables
 *
 * This class manages buildwrappers, scion-browser, etc. It gets the path from the preferences and does the initalization
 *
 * This works by listening for resource changes.
 */
public class ScionManager implements IResourceChangeListener {
  /** Current executable path string */
  private IPath buildWrapperExecutablePath;
  /** Current browser executable path string */
  private IPath browserExecutablePath;
  /** Haskell console low water mark */
  private int hConLowWater;
  /** Haskell console high water mark */
  private int hConHighWater;

  private final static String MINIMUM_BUILDWRAPPER="0.5.0";
  private final static String MINIMUM_SCIONBROWSER="0.2.7";

  public ScionManager() {
    // The interesting stuff is done in the start() method
    buildWrapperExecutablePath = null;
    browserExecutablePath = null;
    hConLowWater = HaskellConsole.HASKELL_CONSOLE_LOW_WATER_MARK;
    hConHighWater = HaskellConsole.HASKELL_CONSOLE_HIGH_WATER_MARK;
  }

  public void start() {
    IWorkspace workSpace = ResourcesPlugin.getWorkspace();
    IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();

    // Capture preferences as currently stored:

    hConLowWater = preferenceStore.getInt( IPreferenceConstants.HASKELL_CONSOLE_LOW_WATER_MARK );
    if (hConLowWater == 0) {
      hConLowWater = HaskellConsole.HASKELL_CONSOLE_LOW_WATER_MARK;
    }
    hConHighWater = preferenceStore.getInt( IPreferenceConstants.HASKELL_CONSOLE_HIGH_WATER_MARK );
    if (hConHighWater == 0) {
      hConHighWater = HaskellConsole.HASKELL_CONSOLE_HIGH_WATER_MARK;
    }

    final String serverExecutable = preferenceStore.getString( IPreferenceConstants.BUILDWRAPPER_EXECUTABLE );
    if (serverExecutable.length() > 0) {
      buildWrapperExecutablePath = new Path(serverExecutable);
      if (!buildWrapperExecutablePath.toFile().exists()){
        buildWrapperExecutablePath=null;
      }
    }
    // look in path
    if (buildWrapperExecutablePath==null){
      File f=FileUtil.findExecutableInPath( "buildwrapper" );
      if (f!=null){
        buildWrapperExecutablePath=new Path(f.getAbsolutePath());
        // set preference
        HaskellUIPlugin.getDefault().getPreferenceStore().setValue(IPreferenceConstants.BUILDWRAPPER_EXECUTABLE,f.getAbsolutePath());
      }
    }

    final String browserExecutable = preferenceStore.getString( IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE );
    if (browserExecutable.length() > 0) {
      browserExecutablePath = new Path(browserExecutable);
      if (!browserExecutablePath.toFile().exists()){
        browserExecutablePath=null;
      }

    }
    // look in path
    if (browserExecutablePath==null){
      File f=FileUtil.findExecutableInPath( "scion-browser" );
      if (f!=null){
        browserExecutablePath=new Path(f.getAbsolutePath());
        // set preference
        HaskellUIPlugin.getDefault().getPreferenceStore().setValue(IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE,f.getAbsolutePath());
      }
    }

    if (buildWrapperExecutablePath==null || browserExecutablePath==null){
      boolean ignore=HaskellUIPlugin.getDefault().getPreferenceStore().getBoolean( IPreferenceConstants.IGNORE_MISSING_EXECUTABLE );
      if (!ignore){
        final Display display = HaskellUIPlugin.getStandardDisplay();
        display.asyncExec( new Runnable() {
          public void run() {
            Shell parent = display.getActiveShell();

            InstallExecutableDialog ied=new InstallExecutableDialog(parent , buildWrapperExecutablePath==null, browserExecutablePath==null );
            ied.open();
          }
        });
      }
    }

    boolean doBuildWrapperSetup=true;
    boolean doBrowserSetup=true;
    boolean ignore=HaskellUIPlugin.getDefault().getPreferenceStore().getBoolean( IPreferenceConstants.IGNORE_TOOOLD_EXECUTABLE );
    if (!ignore){
      final boolean buildwrapperVersionOK=buildWrapperExecutablePath==null || checkVersion( buildWrapperExecutablePath, MINIMUM_BUILDWRAPPER ,true);
      final boolean browserVersionOK=browserExecutablePath==null || checkVersion( browserExecutablePath, MINIMUM_SCIONBROWSER,false );

      doBuildWrapperSetup=buildwrapperVersionOK; // do not launch if too old
      doBrowserSetup=browserVersionOK;// do not launch if too old
      if (!buildwrapperVersionOK || !browserVersionOK){
          final Display display = HaskellUIPlugin.getStandardDisplay();
          display.asyncExec( new Runnable() {
            public void run() {
              Shell parent = display.getActiveShell();

              InstallOutdatedExecutableDialog ied=new InstallOutdatedExecutableDialog(parent , !buildwrapperVersionOK, !browserVersionOK );
              ied.open();
            }
          });

      }
    }
    if (doBuildWrapperSetup){
      buildWrapperFactorySetup();
    }

    // Set up the output logging console for the shared Browser
    HaskellConsole cBrowser = new HaskellConsole(  UITexts.sharedBrowserInstance_console );
    BrowserPlugin.setSharedLogStream( cBrowser.createOutputWriter() );
    cBrowser.setWaterMarks( hConLowWater, hConHighWater );

    if (doBrowserSetup){
      browserSetup();
    }

    String hlint=preferenceStore.getString( IPreferenceConstants.HLINT_EXECUTABLE );
    if (hlint!=null){
      HLintPlugin.setHlintPath( hlint );
    }

    // Sit and listen to the preference store changes
    preferenceStore.addPropertyChangeListener( new ExecutablesPropertiesListener() );

    try {
      workSpace.getRoot().accept( new UpdateResourceVisitor() );
    } catch( CoreException ex ) {
      HaskellUIPlugin.log( UITexts.scion_delta_error, ex );
    }

    workSpace.addResourceChangeListener( this, IResourceChangeEvent.POST_CHANGE );
    workSpace.addResourceChangeListener( new FileDeletionListener(), IResourceChangeEvent.PRE_BUILD );
    // POST_BUILD is similar to POST_CHANGE but the workspace tree is not locked, which is useful for some listeners
    workSpace.addResourceChangeListener( new CabalFileResourceChangeListener(), IResourceChangeEvent.POST_BUILD );
    workSpace.addResourceChangeListener( new ProjectDeletionListener(), IResourceChangeEvent.PRE_DELETE);
  }

  /**
   * Handle ScionPP preference changes.
   *
   * @param forceRebuildServer If using the built-in scion-server, should it be rebuilt?
   * @param forceRebuildServer If using the built-in scion-browser, should it be rebuilt?
   */
  public void handlePreferenceChanges() {
    handlePreferenceChangesServer(  );
    handlePreferenceChangesBrowser(  );
  }

  public void handlePreferenceChangesServer() {
    IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();

    final String newServerExecutable = preferenceStore.getString( IPreferenceConstants.BUILDWRAPPER_EXECUTABLE );
    IPath newServerExecutablePath = new Path(newServerExecutable);

    buildWrapperExecutablePath = newServerExecutablePath;
    buildWrapperFactorySetup();

  }

  public void handlePreferenceChangesBrowser() {
    IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    //boolean newUseBuiltIn = preferenceStore.getBoolean( IPreferenceConstants.SCION_BROWSER_SERVER_BUILTIN );
    final String newServerExecutable = preferenceStore.getString( IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE );
    IPath newServerExecutablePath = new Path(newServerExecutable);

    // Did something change?
    if (newServerExecutablePath.equals( browserExecutablePath )) {
      return;
    }

    // Yup, something changed, so shut down the instances...
    BrowserPlugin.getSharedInstance().stop();

    // Switch over to the null instance factory
    BrowserPlugin.useNullSharedInstance();

    // And update...
    browserExecutablePath = newServerExecutablePath;
    browserSetup();

  }

  /**
   * Server factory setup. This is common code used by both start() and handlePreferenceChanges() for setting
   * and starting scion server factories.
   */
  private synchronized void buildWrapperFactorySetup(){
    IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();

    boolean verbose = preferenceStore.getBoolean( IPreferenceConstants.VERBOSE_INTERACTION );
    BuildWrapperPlugin.logAnswers=verbose;


    if ( buildWrapperExecutablePath != null && buildWrapperExecutablePath.toFile().exists() ) {
      BuildWrapperPlugin.setBwPath( buildWrapperExecutablePath.toOSString() );
    } else {
      BuildWrapperPlugin.setBwPath(null);
      buildWrapperExecutablePath = null;

    }
  }

  public static boolean checkVersion(final IPath path,final String minimal,final boolean wait){
    if (path!=null){
      try {
        String currentVersion=ProcessRunner.getExecutableVersion(path.toOSString(),wait);
        if (currentVersion==null){
          return false;
        }
        return CabalPackageVersion.compare( currentVersion, minimal )>=0;
      } catch (IOException ioe){
        HaskellUIPlugin.log(UITexts.error_checkVersion, ioe);
      }
    }
    return true;
  }



  private synchronized void browserSetup() {
    final Display display = Display.getDefault();

      if ( browserExecutablePath != null && browserExecutablePath.toFile().exists() ) {
        IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
        boolean verbose = preferenceStore.getBoolean( IPreferenceConstants.BROWSER_VERBOSE_INTERACTION );
        BrowserPlugin.changeSharedInstance( browserExecutablePath ,verbose );

        display.asyncExec( new Runnable() {
          public void run() {
            Job builder =  new BrowserLocalDatabaseRebuildJob(UITexts.scionBrowserRebuildingDatabase);
            //builder.setRule( ResourcesPlugin.getWorkspace().getRoot() );
            builder.setPriority( Job.DECORATE );
            builder.schedule();
          }
        } );
      } else {
        browserExecutablePath = null;
        BrowserPlugin.useNullSharedInstance();
      }
  }

  void loadHackageDatabase() {
    final Display display = Display.getDefault();

    final IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    boolean questionWasAnswered = preferenceStore.getBoolean( IPreferenceConstants.SCION_BROWSER_HACKAGE_QUESTION_ANSWERED );
    if( !questionWasAnswered ) {
      display.asyncExec( new Runnable() {
        public void run() {
          // needs ui thread
          final Shell parentShell = display.getActiveShell();
          boolean result = MessageDialog.openQuestion( parentShell,
              UITexts.scionBrowserUseHackage_QuestionNew_title,
              UITexts.scionBrowserUseHackage_QuestionNew_label );
          preferenceStore.setValue( IPreferenceConstants.SCION_BROWSER_HACKAGE_QUESTION_ANSWERED, true );
          preferenceStore.setValue( IPreferenceConstants.SCION_BROWSER_USE_HACKAGE, result );
        }
      } );
    }

    if (preferenceStore.getBoolean( IPreferenceConstants.SCION_BROWSER_USE_HACKAGE )) {
      boolean rebuild = !questionWasAnswered || !BrowserPlugin.getHackageDatabasePath().toFile().exists();
      if (!rebuild) {
        /* Check time of the Hackage database */
        long timeDiff = BrowserPlugin.getHackageDatabasePath().toFile().lastModified() - (new Date()).getTime();
        /* We ask to rebuild if more than one week passed since last update */
        boolean askRebuild = timeDiff > 7 /* days */ * 24 /* h/day */ * 3600 /* s/h */ * 1000 /* ms/s */;
        if (askRebuild) {
          final Stack<Boolean> response = new Stack<Boolean>();
          display.asyncExec( new Runnable() {
            public void run() {
              // needs ui thread
              final Shell parentShell = display.getActiveShell();
              boolean result = MessageDialog.openQuestion( parentShell,
                  UITexts.scionBrowserUseHackage_QuestionUpdate_title,
                  UITexts.scionBrowserUseHackage_QuestionUpdate_label );
              response.push(result);
            }
          } );
          rebuild = response.pop();
        }
      }
      /* Execute build job */
      final boolean doRebuild = rebuild;
      display.asyncExec( new Runnable() {
        public void run() {
        Job builder = new BrowserHackageDatabaseRebuildJob(
            UITexts.scionBrowserRebuildingDatabase, doRebuild );
        // builder.setRule( ResourcesPlugin.getWorkspace().getRoot() );
        builder.setPriority( Job.DECORATE );
        builder.schedule();
        }
      } );
    } else {
      preloadPrelude();
      checkHoogleDataIsPresent();
    }
  }

  void preloadPrelude() {
    try {
      BrowserPlugin.getSharedInstance().getDeclarations(Database.ALL, "Prelude" );
    } catch (Exception e) {
      // Do nothing
    }
  }

  void checkHoogleDataIsPresent() {
    final IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    String extraHooglePath = preferenceStore.getString( IPreferenceConstants.SCION_BROWSER_EXTRA_HOOGLE_PATH );
    checkHoogleDataIsPresent(extraHooglePath);

  }

  void checkHoogleDataIsPresent(final String extraHooglePath) {
    boolean rebuild = false;
    try {
      // Set extra paths if needed
     if (extraHooglePath != null && extraHooglePath.length() > 0) {
        BrowserPlugin.getSharedInstance().setExtraHooglePath( extraHooglePath );
      }
      rebuild = !BrowserPlugin.getSharedInstance().checkHoogle();
    } catch( Exception e ) {
      rebuild = true;
    }
    if( rebuild ) {
      // There is no "fmap", we don't have a database
      final Display display = Display.getDefault();

      display.asyncExec( new Runnable() {

        public void run() {
          // needs ui thread
          Shell parentShell = display.getActiveShell();
          if( MessageDialog
              .openQuestion(
                  parentShell,
                  UITexts.hoogle_dataNotPresent_title,
                  UITexts.hoogle_dataNotPresent_message ) ) {
            display.asyncExec( new Runnable() {

              public void run() {
                Job builder = new HoogleDownloadDataJob(
                    UITexts.hoogle_downloadingData );
                builder.setRule( ResourcesPlugin.getWorkspace().getRoot() );
                builder.setPriority( Job.DECORATE );
                builder.schedule();
              }
            } );
          }
        }
      } );
    }
  }


  /**
   * Detects when a file is deleted and updates the Cabal file accordingly (remove the module).
   * If the removed file is the cabal file, stop the underlying scion-server.
   *
   * @author JP Moresmau
   */
  private class FileDeletionListener implements IResourceChangeListener {

    public void resourceChanged( final IResourceChangeEvent event ) {
      try {
        event.getDelta().accept( new IResourceDeltaVisitor() {

          public boolean visit( final IResourceDelta delta )
              throws CoreException {
            if( delta.getKind() == IResourceDelta.REMOVED ) {
              if( delta.getResource() instanceof IFile){
                IFile f = ( IFile )delta.getResource();
                if (!f.getProject().isOpen()){
                  return false;
                }
                IFile cabalF = BuildWrapperPlugin.getCabalFile( f.getProject() );
                if(FileUtil.hasHaskellExtension( f ) && f.getProject().isOpen()) {
                  // System.out.println(delta.getFullPath());

                  PackageDescription pd = PackageDescriptionLoader.load( cabalF );
                  ModuleCreationInfo info = new ModuleCreationInfo( f );
                  if (info.getSourceContainer()!=null){
                    List<PackageDescriptionStanza> lpds = pd.getStanzasBySourceDir().get( info.getSourceContainer().getProjectRelativePath().toOSString() );
                    if (lpds!=null && lpds.size()>0){
                      String qn = info.getQualifiedModuleName();

                      IDocumentProvider prov = new TextFileDocumentProvider();
                      prov.connect( cabalF );
                      try {
                        IDocument doc = prov.getDocument( cabalF );

                        for( PackageDescriptionStanza pds: lpds ) {
                          pds=pd.getSameStanza(pds);
                          RealValuePosition rvp = pds.removeFromPropertyList( CabalSyntax.FIELD_EXPOSED_MODULES, qn );
                          if (rvp!=null){
                            rvp.updateDocument( doc );
                            pd=PackageDescriptionLoader.load( doc.get() );
                            pds=pd.getSameStanza(pds);
                          }
                          rvp = pds.removeFromPropertyList( CabalSyntax.FIELD_OTHER_MODULES, qn );
                          if (rvp!=null){
                            rvp.updateDocument( doc );
                            pd=PackageDescriptionLoader.load( doc.get() );
                          }
                        }
                        prov.saveDocument( null, cabalF, doc, true );
                      } finally {
                        prov.disconnect( cabalF );
                      }
                    }
                  }
                  /* final ScionInstance si = ScionPlugin.getScionInstance( f );
                  if (si != null) {
                    BuildOptions buildOptions=new BuildOptions().setOutput(false).setRecompile(true);
                    si.buildProject( buildOptions);
                  }*/
                  JobFacade fa=BuildWrapperPlugin.getJobFacade( f.getProject() );
                  if (fa!=null){
                    fa.build( new BuildOptions().setOutput(false) );
                  }

                  return false;
                } else if (f.equals( cabalF )){
                    stopInstance( f );
                    return false;

                }
              }

            }

            return true;
          }
        } );

      } catch( CoreException ex ) {
        HaskellUIPlugin.log( UITexts.scion_delta_error, ex );
      }
    }
  }


  /**
 * <p>detects when a haskell project is deleted and stops the corresponding scion server</p>
  *
  * @author JP Moresmau
 */
  public class ProjectDeletionListener implements IResourceChangeListener{
    public void resourceChanged( final IResourceChangeEvent event ) {
      if (event.getResource() instanceof IProject){
        stopInstance( event.getResource() );
      }
    }
  }

  /** */
  public class ExecutablesPropertiesListener implements IPropertyChangeListener {
    public void propertyChange( final PropertyChangeEvent event ) {
          if( event.getProperty().equals( IPreferenceConstants.BUILDWRAPPER_EXECUTABLE ) ) {
            if( event.getNewValue() instanceof String ) {
              buildWrapperExecutablePath=new Path((String)event.getNewValue());
              buildWrapperFactorySetup();
            }
          } else if (event.getProperty().equals( IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE)) {
              if (event.getNewValue() instanceof String) {
                browserExecutablePath = new Path((String)event.getNewValue());
                browserSetup();
              }
          } else if (event.getProperty().equals( IPreferenceConstants.SCION_BROWSER_EXTRA_HOOGLE_PATH)) {
            if (event.getNewValue() instanceof String) {
              checkHoogleDataIsPresent((String)event.getNewValue());
            }
          } else if (event.getProperty().equals(IPreferenceConstants.HLINT_EXECUTABLE)){
            if (event.getNewValue() instanceof String){
              HLintPlugin.setHlintPath( (String)event.getNewValue() );
            }
          }

    }
  }

  /** */
  public class UpdateResourceVisitor implements IResourceVisitor {
    public boolean visit( final IResource resource ) throws CoreException {
      return updateForResource( resource );
    }
  }

  /** */
  public class CabalFileResourceChangeListener implements IResourceChangeListener {
    public void resourceChanged( final IResourceChangeEvent event ) {
      try {
        event.getDelta().accept( new IResourceDeltaVisitor() {

          public boolean visit( final IResourceDelta delta ) {
            if( delta.getKind() == IResourceDelta.CHANGED && (delta.getFlags() & IResourceDelta.CONTENT)>0) {
              if( delta.getResource() instanceof IFile ) {
                IFile f = ( IFile )delta.getResource();
                IFile cabalF = BuildWrapperPlugin.getCabalFile( f.getProject() );
                if( f.equals( cabalF ) ) {
                  BWFacade bwf=BuildWrapperPlugin.getFacade( f.getProject() );
                  if (bwf!=null){
                    bwf.cabalFileChanged();
                  }
                  for (CabalFileChangeListener l:CabalFileChangeListenerManager.getListeners()){
                    l.cabalFileChanged( f );
                  }

                }
                return false;
              }
            }
            return true;

          }
        } );

      } catch( CoreException ex ) {
        HaskellUIPlugin.log( UITexts.scion_delta_error, ex );
      }
    }
  }

  /** Preference change listener when EclipseFP core preferences change */
//  public class CorePreferencesChangeListener implements IPreferenceChangeListener {
//    public void preferenceChange( final PreferenceChangeEvent event ) {
//      String key = event.getKey();
//      //if(    ICorePreferenceNames.HS_IMPLEMENTATIONS.equals( key )
//      //    || ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION.equals( key ) ) {
//      if (CabalImplementationManager.DEFAULT_CABAL_IMPLEMENTATION.equals( key ) ||  ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION.equals( key )){
//        //if (useBuiltIn){
//          handlePreferenceChanges();
//        //}
//      } /*else {
//        HaskellUIPlugin.log("Core preference changed: ".concat( key ), IStatus.INFO);
//      }*/
//    }
//  }

  public void stop() {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );
 //   ScionPlugin.stopAllInstances();
  }

  /**
   * Called after a resource in the workspace was changed. It finds all projects
   * that were opened/closed, and starts/stops Scion instances accordingly.
   */
  public void resourceChanged( final IResourceChangeEvent event ) {
    try {
      event.getDelta().accept( new IResourceDeltaVisitor() {
        public boolean visit( final IResourceDelta delta ) throws CoreException {
          return updateForResource( delta.getResource() );
        }
      } );
    } catch( CoreException ex ) {
      HaskellUIPlugin.log( UITexts.scion_delta_error, ex );
    }
  }

  private boolean updateForResource( final IResource resource )
      throws CoreException {
    if( resource instanceof IProject ) {
      IProject project = ( IProject ) resource;
      if(    project.isOpen()
          && project.hasNature( HaskellNature.NATURE_ID ) ) {
          startInstance( project );
      }
      if( !project.isOpen() ) {
        // we cannot check the nature of closed projects, but if it's in
        // instances, stop it
        stopInstance( project );
      }
      return true; // projects can't be children of other projects, can they?
    }
    return true;
  }

  /**
   * Starts and returns a new Scion instance for the given project. Does not add
   * the instance to the instances map.
   */
  private synchronized void startInstance( final IProject project ) {
    if (BuildWrapperPlugin.getFacade( project )==null){
      HaskellConsole cbw=getBWHaskellConsole( project );
      Writer outStreamBw = cbw.createOutputWriter();

      BuildWrapperPlugin.createFacade(project, CabalImplementationManager.getCabalExecutable(), outStreamBw );
    }
  }



  /**
   * Stops the Scion instance for the given project. Does not remove the
   * instance from the instances map.
   */
  private void stopInstance( final IProject project ) {
    if( project != null) {
      BuildWrapperPlugin.removeFacade( project );
      //if ( ScionPlugin.terminateScionInstance( project ) ) {
        IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
        String name = bwconsoleName( project);
        for( IConsole c: mgr.getConsoles() ) {
          if( c.getName().equals( name ) ) {
            mgr.removeConsoles( new IConsole[] { c } );
            break;
          }
        }
      //}
    }
  }

  private void stopInstance(final IResource res){
    if (res != null && res.getProject() != null) {
      stopInstance( res.getProject() );
    }
  }



  private final String bwconsoleName ( final IProject project ) {
    String projectName = project != null ? project.getName() : UITexts.noproject;
    return NLS.bind( UITexts.bw_console_title, projectName );
  }

  private HaskellConsole getBWHaskellConsole(final IProject project) {
    final String consoleName = bwconsoleName(project);
    final IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();

    for( IConsole c: mgr.getConsoles() ) {
      if( c.getName().equals( consoleName ) ) {
        return (HaskellConsole) c;
      }
    }

    HaskellConsole hCon = new HaskellConsole( consoleName );

    hCon.setWaterMarks( hConLowWater, hConHighWater );
    return hCon;
  }



  /** Specialized Job class that manages rebuilding the Browser database.
   *  Based in the work of B. Scott Michel.
   *
   * @author B. Alejandro Serrano
   */
  public class BrowserLocalDatabaseRebuildJob extends Job {
    IStatus status;

    public BrowserLocalDatabaseRebuildJob(final String jobTitle) {
      super(jobTitle);

      // If the build failed, there will be some indication of why it failed.
      addJobChangeListener( new JobChangeAdapter() {
        @Override
        public void done( final IJobChangeEvent event ) {
          if (event.getResult().isOK()) {
            loadHackageDatabase();
          } else {
            Display.getDefault().syncExec( new Runnable() {
              public void run() {
                MessageDialog.openError( Display.getDefault().getActiveShell(),
                                         UITexts.scionBrowserRebuildingDatabaseError_title,
                                         UITexts.scionBrowserRebuildingDatabaseError_message );
              }
            } );
          }

          super.done( event );
        }
      });
    }

    @Override
    protected IStatus run( final IProgressMonitor monitor ) {
      monitor.beginTask( UITexts.scionBrowserRebuildingDatabase, IProgressMonitor.UNKNOWN );
      status = BrowserPlugin.loadLocalDatabase( true );
      monitor.done();

      return status;
    }
  }

  /** Specialized Job class that manages loading the Hackage part of Browser database.
   *  Based in the work of B. Scott Michel.
   *
   * @author B. Alejandro Serrano
   */
  public class BrowserHackageDatabaseRebuildJob extends Job {
    IStatus status;
    boolean rebuild;

    public BrowserHackageDatabaseRebuildJob(final String jobTitle, final boolean rebuild) {
      super(jobTitle);
      this.rebuild = rebuild;

      // If the build failed, there will be some indication of why it failed.
      addJobChangeListener( new JobChangeAdapter() {
        @Override
        public void done( final IJobChangeEvent event ) {
          if (event.getResult().isOK()) {
            preloadPrelude();
            checkHoogleDataIsPresent();
          } else {
            Display.getDefault().syncExec( new Runnable() {
              public void run() {
                MessageDialog.openError( Display.getDefault().getActiveShell(),
                                         UITexts.scionBrowserRebuildingDatabaseError_title,
                                         UITexts.scionBrowserRebuildingDatabaseError_message );
              }
            } );
          }

          super.done( event );
        }
      });
    }

    @Override
    protected IStatus run( final IProgressMonitor monitor ) {
      monitor.beginTask( UITexts.scionBrowserRebuildingDatabase, IProgressMonitor.UNKNOWN );
      status = BrowserPlugin.loadHackageDatabase( rebuild );
      monitor.done();

      return status;
    }
  }

  /** Specialized Job class that manages downloading Hoogle data.
   *  Based in the work of B. Scott Michel.
   *
    * @author B. Alejandro Serrano
   */
  public class HoogleDownloadDataJob extends Job {

    public HoogleDownloadDataJob( final String jobTitle ) {
      super( jobTitle );
    }

    @Override
    protected IStatus run( final IProgressMonitor monitor ) {
      monitor.beginTask( UITexts.hoogle_downloadingData, IProgressMonitor.UNKNOWN );
      try {
        BrowserPlugin.getSharedInstance().downloadHoogleData();
        BrowserPlugin.getSharedInstance().checkHoogle();
      } catch( Exception e ) {
        // Do nothing if fails
      }
      monitor.done();

      return Status.OK_STATUS;
    }
  }
}
