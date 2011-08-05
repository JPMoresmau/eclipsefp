package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.io.File;
import java.io.Writer;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.browser.BrowserPlugin;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementation;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescription;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionLoader;
import net.sf.eclipsefp.haskell.core.cabalmodel.PackageDescriptionStanza;
import net.sf.eclipsefp.haskell.core.cabalmodel.RealValuePosition;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.scion.client.CabalComponentResolver;
import net.sf.eclipsefp.haskell.scion.client.IScionEventListener;
import net.sf.eclipsefp.haskell.scion.client.ScionEvent;
import net.sf.eclipsefp.haskell.scion.client.ScionEventType;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.scion.types.BuildOptions;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.console.HaskellConsole;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ScionPP;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.CabalFileChangeListener;
import net.sf.eclipsefp.haskell.util.FileUtil;
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
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IOConsole;
import org.eclipse.ui.console.IOConsoleOutputStream;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.ui.statushandlers.StatusManager;
import org.eclipse.ui.texteditor.IDocumentProvider;

/**
 * Manages instances of Scion servers.
 *
 * This class ensures that there is exactly one running Scion instance for each
 * open project. This instance can be accessed through
 * {@link #getScionInstance(IResource)}.
 *
 * This works by listening for resource changes.
 */
public class ScionManager implements IResourceChangeListener, IScionEventListener {
  /** Preference value for using the standard stream-based connection to the scion-server */
  public final static String STDSTREAM_SCION_FLAVOR = "stdstream";
  /** Preference value for using the network-based connection to the scion-server */
  public final static String NETWORK_SCION_FLAVOR = "network";
  /** Current "use builtin" state */
  private boolean useBuiltIn;
  /** Current server flavor */
  private String serverFlavor;
  /** Current executable path string */
  private IPath serverExecutablePath;
  /** Current "use builtin" state */
  private boolean browserUseBuiltIn;
  /** Current browser executable path string */
  private IPath browserExecutablePath;
  /** Haskell console low water mark */
  private int hConLowWater;
  /** Haskell console high water mark */
  private int hConHighWater;

  /** The Job that builds the built-in scion-server, when required. This prevents multiple build jobs from
   * being fired off.
   */
  private ScionBuildJob internalBuilder;
  private BrowserBuildJob internalBrowserBuilder;

  public ScionManager() {
    // The interesting stuff is done in the start() method
    useBuiltIn = true;
    browserUseBuiltIn = true;
    serverFlavor = STDSTREAM_SCION_FLAVOR;
    serverExecutablePath = null;
    browserExecutablePath = null;
    internalBuilder = null;
    internalBrowserBuilder = null;
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

    useBuiltIn = preferenceStore.getBoolean( IPreferenceConstants.SCION_SERVER_BUILTIN );
    serverFlavor = preferenceStore.getString( IPreferenceConstants.SCION_SERVER_FLAVOR );
    // If the server flavor isn't set, default to standard stream.
    if (serverFlavor.length() == 0) {
      serverFlavor = STDSTREAM_SCION_FLAVOR;
    }

    final String serverExecutable = preferenceStore.getString( IPreferenceConstants.SCION_SERVER_EXECUTABLE );
    if (serverExecutable.length() > 0) {
      serverExecutablePath = new Path(serverExecutable);
    }

    browserUseBuiltIn = preferenceStore.getBoolean( IPreferenceConstants.SCION_BROWSER_SERVER_BUILTIN );
    final String browserExecutable = preferenceStore.getString( IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE );
    if (browserExecutable.length() > 0) {
      browserExecutablePath = new Path(browserExecutable);
    }

    // Set up the output logging console for the shared ScionInstance:
    HaskellConsole c = new HaskellConsole( UITexts.sharedScionInstance_console );
    ScionPlugin.setSharedInstanceWriter( c.createOutputWriter() );
    c.setWaterMarks( hConLowWater, hConHighWater );

    serverFactorySetup();

    // Set up the output logging console for the shared Browser
    HaskellConsole cBrowser = new HaskellConsole(  UITexts.sharedBrowserInstance_console );
    BrowserPlugin.setSharedLogStream( cBrowser.createOutputWriter() );
    cBrowser.setWaterMarks( hConLowWater, hConHighWater );

    browserSetup();

    // Sit and listen to the core preference store changes
    IEclipsePreferences instanceScope = HaskellCorePlugin.instanceScopedPreferences();
    instanceScope.addPreferenceChangeListener( new CorePreferencesChangeListener() );
    preferenceStore.addPropertyChangeListener( new ScionServerPropertiesListener() );

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
  public void handlePreferenceChanges(final boolean forceRebuildServer, final boolean forceRebuildBrowser) {
    handlePreferenceChangesServer( forceRebuildServer );
    handlePreferenceChangesBrowser( forceRebuildBrowser );
  }

  public void handlePreferenceChangesServer(final boolean forceRebuild) {
    IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    boolean newUseBuiltIn = preferenceStore.getBoolean( IPreferenceConstants.SCION_SERVER_BUILTIN );
    String newServerFlavor = preferenceStore.getString( IPreferenceConstants.SCION_SERVER_FLAVOR );
    final String newServerExecutable = preferenceStore.getString( IPreferenceConstants.SCION_SERVER_EXECUTABLE );
    IPath newServerExecutablePath = new Path(newServerExecutable);

    // Did any of the major properties change, and we're not forcing a rebuild?
    if (   !forceRebuild
        && newUseBuiltIn == useBuiltIn
        && newServerFlavor.equals( serverFlavor )
        && (useBuiltIn || newServerExecutablePath.equals( serverExecutablePath ))) {
      return;
    }

    // Yup, something changed, so shut down the instances...
    ScionPlugin.shutdownAllInstances();

    // Switch over to the null instance factory
    try {
      ScionPlugin.useNullScionServerFactory();
    } catch (ScionServerStartupException ex) {
      // Should never get generated, but make Java happy.
    }

    // And update...
    useBuiltIn = newUseBuiltIn;
    serverFlavor = newServerFlavor;
    serverExecutablePath = newServerExecutablePath;

    // Can we forcibly rebuild (must use builtin, have Haskell and Cabal implementations
    if ( forceRebuild ) {
      if ( useBuiltIn
          && CompilerManager.getInstance().getCurrentHsImplementation() != null
          && CabalImplementationManager.getInstance().getDefaultCabalImplementation() != null) {
        final Display display = HaskellUIPlugin.getStandardDisplay();
        display.asyncExec( new Runnable() {
          public void run() {
            Shell parent = display.getActiveShell();
            if ( MessageDialog.openConfirm( parent, UITexts.scionRebuild_title, UITexts.scionRebuild_message ) ) {
              IPath scionBuildDirPath = ScionPlugin.builtinServerDirectoryPath();
              File scionBuildDir = scionBuildDirPath.toFile();

              FileUtil.deleteRecursively( scionBuildDir );
              if ( scionBuildDir.exists() ) {
                MessageDialog.openError( parent, UITexts.scionRebuild_DirectoryExists_title,
                                         UITexts.scionRebuild_DirectoryExists_message );
              } else {
                serverFactorySetup();
              }
            }
          }
        } );
      }
    } else {
      // Everything else is handled by serverFactorySetup...
      serverFactorySetup();
    }
  }

  public void handlePreferenceChangesBrowser(final boolean forceRebuild) {
    IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    boolean newUseBuiltIn = preferenceStore.getBoolean( IPreferenceConstants.SCION_BROWSER_SERVER_BUILTIN );
    final String newServerExecutable = preferenceStore.getString( IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE );
    IPath newServerExecutablePath = new Path(newServerExecutable);

    // Did any of the major properties change, and we're not forcing a rebuild?
    if (   !forceRebuild
        && newUseBuiltIn == browserUseBuiltIn
        && (browserUseBuiltIn || newServerExecutablePath.equals( browserExecutablePath ))) {
      return;
    }

    // Yup, something changed, so shut down the instances...
    BrowserPlugin.getSharedInstance().stop();

    // Switch over to the null instance factory
    BrowserPlugin.useNullSharedInstance();

    // And update...
    browserUseBuiltIn = newUseBuiltIn;
    browserExecutablePath = newServerExecutablePath;

    // Can we forcibly rebuild (must use builtin, have Haskell and Cabal implementations
    if ( forceRebuild ) {
      if ( browserUseBuiltIn
          && CompilerManager.getInstance().getCurrentHsImplementation() != null
          && CabalImplementationManager.getInstance().getDefaultCabalImplementation() != null) {
        final Display display = HaskellUIPlugin.getStandardDisplay();
        display.asyncExec( new Runnable() {
          public void run() {
            Shell parent = display.getActiveShell();
            if ( MessageDialog.openConfirm( parent, UITexts.browserRebuild_title, UITexts.browserRebuild_message ) ) {
              IPath browserBuildDirPath = BrowserPlugin.builtinServerDirectoryPath();
              File browserBuildDir = browserBuildDirPath.toFile();

              FileUtil.deleteRecursively( browserBuildDir );
              if ( browserBuildDir.exists() ) {
                MessageDialog.openError( parent, UITexts.browserRebuild_DirectoryExists_title,
                                         UITexts.browserRebuild_DirectoryExists_message );
              } else {
                browserSetup();
              }
            }
          }
        } );
      }
    } else {
      // Everything else is handled by browserSetup...
      browserSetup();
    }
  }

  /**
   * Server factory setup. This is common code used by both start() and handlePreferenceChanges() for setting
   * and starting scion server factories.
   */
  private synchronized void serverFactorySetup()
  {
    try {
      if (useBuiltIn) {
        if (   CompilerManager.getInstance().getCurrentHsImplementation() != null
            && CabalImplementationManager.getInstance().getDefaultCabalImplementation() != null) {
            if ( !ScionBuilder.scionNeedsBuilding() ) {
              if (STDSTREAM_SCION_FLAVOR.equals( serverFlavor )) {
                ScionPlugin.useBuiltInStdStreamServerFactory();
              } else {
                ScionPlugin.useBuiltInNetworkServerFactory();
              }
            } else {
              spawnBuildJob();
            }
        }
      } else {
        if ( serverExecutablePath != null && serverExecutablePath.toFile().exists() ) {
          if (STDSTREAM_SCION_FLAVOR.equals( serverFlavor )) {
            ScionPlugin.useStdStreamScionServerFactory( serverExecutablePath );
          } else {
            ScionPlugin.useNetworkStreamScionServerFactory( serverExecutablePath );
          }
        } else {
          final Display display = Display.getDefault();

          display.asyncExec( new Runnable() {
            public void run() {
              // needs ui thread
              Shell parentShell = display.getActiveShell();
              String errMsg = NLS.bind( UITexts.scionServerDoesntExist_message, serverExecutablePath.toOSString() );
              MessageDialog.openError( parentShell, UITexts.scionServerDoesntExist_title, errMsg );
            }
          } );

          serverExecutablePath = null;
          ScionPlugin.useNullScionServerFactory();
        }
      }
    } catch (ScionServerStartupException ex) {
      reportServerStartupError( ex );
    }
  }

  /**
   * Build the built-in scion-server: unpack the internal scion-x.y.z.a.zip archive into the destination
   * folder, then kick of a "cabal install" compilation.
   *
   * @param monitor The progress monitor
   * @param conout The console output stream
   */
  private ScionBuildStatus buildBuiltIn(final IProgressMonitor monitor, final IOConsoleOutputStream conout) {
    IPath scionBuildDirPath = ScionPlugin.builtinServerDirectoryPath();
    File scionBuildDir = scionBuildDirPath.toFile();
    ScionBuilder builder = new ScionBuilder();
    ScionBuildStatus retval;

    monitor.subTask( UITexts.scionServerProgress_subtask1 );
    retval = builder.unpackScionArchive( scionBuildDir );
    if (retval.isOK()) {

      // build final exe location
      IHsImplementation hsImpl = CompilerManager.getInstance().getCurrentHsImplementation();
      CabalImplementationManager cabalMgr = CabalImplementationManager.getInstance();
      CabalImplementation cabalImpl = cabalMgr.getDefaultCabalImplementation();

      IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
      boolean updateCabal = preferenceStore.getBoolean( IPreferenceConstants.RUN_CABAL_UPDATE );
      if (updateCabal){
        preferenceStore.setValue( IPreferenceConstants.RUN_CABAL_UPDATE, false );
        monitor.subTask( UITexts.cabalUpdateProgress );
        builder.update( cabalImpl, conout );
        // we ignore the return so that failing update does not stop the compilation
      }

      IPath exePath = ScionPlugin.serverExecutablePath( scionBuildDirPath );
      File  exeFile = exePath.toFile();

      if( !exeFile.exists() && hsImpl != null && cabalImpl != null) {
        monitor.subTask( UITexts.scionServerProgress_subtask2 );
        retval = builder.build( cabalImpl, scionBuildDir, conout);
        if (retval.isOK() && exeFile.exists() ) {
          retval.setExecutable( exePath.toOSString() );
        }
      } else {
        if (cabalImpl == null ) {
          retval.buildFailed(UITexts.noCabalImplementation_title, UITexts.noCabalImplementation_message);
        }
      }
    }
    return retval;
  }

  private synchronized void browserSetup() {
    final Display display = Display.getDefault();

    if (browserUseBuiltIn) {
      if (   CompilerManager.getInstance().getCurrentHsImplementation() != null
          && CabalImplementationManager.getInstance().getDefaultCabalImplementation() != null) {
          if ( !BrowserBuilder.browserNeedsBuilding() ) {
            BrowserPlugin.useSharedBuiltinInstance();

            display.asyncExec( new Runnable() {
              public void run() {
                Job builder =  new BrowserDatabaseRebuildJob(UITexts.scionBrowserRebuildingDatabase);
                builder.setPriority( Job.BUILD );
                builder.setRule( ResourcesPlugin.getWorkspace().getRoot() );
                builder.setUser(true);
                builder.schedule();
              }
            } );
          } else {
            spawnBrowserBuildJob();
          }
      }
    } else {
      if ( browserExecutablePath != null && browserExecutablePath.toFile().exists() ) {
        BrowserPlugin.changeSharedInstance( browserExecutablePath );

        display.asyncExec( new Runnable() {
          public void run() {
            Job builder =  new BrowserDatabaseRebuildJob(UITexts.scionBrowserRebuildingDatabase);
            builder.setPriority( Job.BUILD );
            builder.setRule( ResourcesPlugin.getWorkspace().getRoot() );
            builder.setUser(true);
            builder.schedule();
          }
        } );
      } else {
        display.asyncExec( new Runnable() {
          public void run() {
            // needs ui thread
            Shell parentShell = display.getActiveShell();
            MessageDialog.openError( parentShell, UITexts.scionBrowserNotConfigured_title, UITexts.scionBrowserNotConfigured_message );
          }
        } );

        browserExecutablePath = null;
        BrowserPlugin.useNullSharedInstance();
      }
    }
  }

  void checkHoogleDataIsPresent() {
    boolean rebuild = false;
    try {
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
                builder.setPriority( Job.BUILD );
                builder.setRule( ResourcesPlugin.getWorkspace().getRoot() );
                builder.setUser( true );
                builder.schedule();
              }
            } );
          }
        }
      } );
    }
  }

  /**
   * Build the built-in scion-server: unpack the internal scion-browser-x.y.z.a.zip archive into the destination
   * folder, then kick of a "cabal install" compilation.
   *
   * @param monitor The progress monitor
   * @param conout The console output stream
   */
  private ScionBuildStatus buildBrowserBuiltIn(final IProgressMonitor monitor, final IOConsoleOutputStream conout) {
    IPath browserBuildDirPath = BrowserPlugin.builtinServerDirectoryPath();
    File browserBuildDir = browserBuildDirPath.toFile();
    BrowserBuilder builder = new BrowserBuilder();
    ScionBuildStatus retval;

    monitor.subTask( UITexts.browserServerProgress_subtask1 );
    retval = builder.unpackScionArchive( browserBuildDir );
    if (retval.isOK()) {
      // build final exe location
      IHsImplementation hsImpl = CompilerManager.getInstance().getCurrentHsImplementation();
      CabalImplementationManager cabalMgr = CabalImplementationManager.getInstance();
      CabalImplementation cabalImpl = cabalMgr.getDefaultCabalImplementation();

      IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
      boolean updateCabal = preferenceStore.getBoolean( IPreferenceConstants.RUN_CABAL_UPDATE );
      if (updateCabal){
        preferenceStore.setValue( IPreferenceConstants.RUN_CABAL_UPDATE, false );
        monitor.subTask( UITexts.cabalUpdateProgress );
        builder.update( cabalImpl, conout );
        // we ignore the return so that failing update does not stop the compilation
      }

      IPath exePath = BrowserPlugin.serverExecutablePath( browserBuildDirPath );
      File  exeFile = exePath.toFile();

      if( !exeFile.exists() && hsImpl != null && cabalImpl != null) {
        monitor.subTask( UITexts.browserServerProgress_subtask2 );
        retval = builder.build( cabalImpl, browserBuildDir, conout);
        if (retval.isOK() && exeFile.exists() ) {
          retval.setExecutable( exePath.toOSString() );
        }
      } else {
        if (cabalImpl == null ) {
          retval.buildFailed(UITexts.noCabalImplementation_title, UITexts.noCabalImplementation_message);
        }
      }
    }
    return retval;
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
                IFile cabalF = ScionInstance.getCabalFile( f.getProject() );
                if(FileUtil.hasHaskellExtension( f ) && f.getProject().isOpen()) {
                  // System.out.println(delta.getFullPath());

                  PackageDescription pd = PackageDescriptionLoader.load( cabalF );
                  ModuleCreationInfo info = new ModuleCreationInfo( f );
                  if (info.getSourceContainer()!=null){
                    List<PackageDescriptionStanza> lpds = pd.getStanzasBySourceDir().get( info.getSourceContainer().getProjectRelativePath().toOSString() );
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
                  final ScionInstance si = ScionPlugin.getScionInstance( f );
                  if (si != null) {
                    BuildOptions buildOptions=new BuildOptions().setOutput(false).setRecompile(true);
                    si.buildProject( buildOptions);
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
  public class ScionServerPropertiesListener implements IPropertyChangeListener {
    public void propertyChange( final PropertyChangeEvent event ) {
      try {
        IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
        // built in state has changed
        if (event.getProperty().equals( IPreferenceConstants.SCION_SERVER_BUILTIN )) {
          if (event.getNewValue() instanceof Boolean) {
            // true -> build
            if (((Boolean)event.getNewValue()).booleanValue()) {
              spawnBuildJob();
            } else {
              // false: read user-specified server executable property
              String serverExecutable =  preferenceStore.getString( IPreferenceConstants.SCION_SERVER_EXECUTABLE );
              IPath serverExecutablePath = new Path(serverExecutable);
              String serverFlavor = preferenceStore.getString( IPreferenceConstants.SCION_SERVER_FLAVOR );

              if (serverFlavor.length() == 0) {
                serverFlavor = STDSTREAM_SCION_FLAVOR;
              }

              if (STDSTREAM_SCION_FLAVOR.equals( serverFlavor )) {
                ScionPlugin.useStdStreamScionServerFactory( serverExecutablePath );
              } else {
                ScionPlugin.useNetworkStreamScionServerFactory( serverExecutablePath );
              }
            }
          }
          // if we're not using built in
        } else if (!preferenceStore.getBoolean( IPreferenceConstants.SCION_SERVER_BUILTIN ) ) {
          if( event.getProperty().equals( IPreferenceConstants.SCION_SERVER_EXECUTABLE ) ) {
            if( event.getNewValue() instanceof String ) {
              String serverExecutable = ( String )event.getNewValue();
              ScionPlugin.useStdStreamScionServerFactory( new Path(serverExecutable) );
            }
          }
        }
      }  catch (ScionServerStartupException ex) {
        reportServerStartupError( ex );
      }
    }
  }

  public class ScionBrowserServerPropertiesListener implements IPropertyChangeListener {
    public void propertyChange( final PropertyChangeEvent event ) {
      try {
        if (event.getProperty().equals( IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE)) {
          if (event.getNewValue() instanceof String) {
            browserExecutablePath = new Path((String)event.getNewValue());
            browserSetup();
          }
        }
      } catch (Throwable ex) {
        // Do nothing
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
            if( delta.getKind() == IResourceDelta.CHANGED ) {
              if( delta.getResource() instanceof IFile ) {
                IFile f = ( IFile )delta.getResource();
                IFile cabalF = ScionInstance.getCabalFile( f.getProject() );
                if( f.equals( cabalF ) ) {
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
  public class CorePreferencesChangeListener implements IPreferenceChangeListener {
    public void preferenceChange( final PreferenceChangeEvent event ) {
      String key = event.getKey();
      //if(    ICorePreferenceNames.HS_IMPLEMENTATIONS.equals( key )
      //    || ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION.equals( key ) ) {
      if (CabalImplementationManager.DEFAULT_CABAL_IMPLEMENTATION.equals( key ) ||  ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION.equals( key )){
        if (useBuiltIn){
          handlePreferenceChanges(true, true);
        }
      } /*else {
        HaskellUIPlugin.log("Core preference changed: ".concat( key ), IStatus.INFO);
      }*/
    }
  }

  public void stop() {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );
    ScionPlugin.stopAllInstances();
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
  private synchronized ScionInstance startInstance( final IProject project ) {
    ScionInstance instance = ScionPlugin.getScionInstance( project );

    if ( instance == null ) {
      HaskellConsole c = getHaskellConsole( project );
      Writer outStream = c.createOutputWriter();

      instance = ScionPlugin.createScionInstance( project, outStream,
          new CabalComponentResolver() {
            public Set<String> getComponents( final IFile file ) {
              Set<PackageDescriptionStanza> pds= ResourceUtil.getApplicableStanzas( new IFile[]{file} );
              Set<String> ret=new HashSet<String>(pds.size());
              for (PackageDescriptionStanza pd:pds){
                ret.add(pd.toTypeName());
              }
              return ret;
            }
          } );
      try {
        instance.start();
        instance.addListener( this );
      } catch( ScionServerStartupException ex ) {
        reportServerStartupError( ex );
      }
    }

    return instance;
  }

  /**
   * Stops the Scion instance for the given project. Does not remove the
   * instance from the instances map.
   */
  private void stopInstance( final IProject project ) {
    if( project != null) {
      if ( ScionPlugin.terminateScionInstance( project ) ) {
        IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
        String name = consoleName( project);
        for( IConsole c: mgr.getConsoles() ) {
          if( c.getName().equals( name ) ) {
            mgr.removeConsoles( new IConsole[] { c } );
            break;
          }
        }
      }
    }
  }

  private void stopInstance(final IResource res){
    if (res != null && res.getProject() != null) {
      stopInstance( res.getProject() );
    }
  }

  private void reportServerStartupError( final ScionServerStartupException ex ) {
    IStatus status = new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), ex.getMessage(), ex );
    StatusManager.getManager().handle( status, StatusManager.LOG );
    HaskellUIPlugin.getStandardDisplay().asyncExec( new Runnable() {
      public void run() {
        Shell parent = HaskellUIPlugin.getStandardDisplay().getActiveShell();
        String text = NLS.bind( UITexts.scionServerStartupError_message, ScionPlugin.getFactoryExecutablePath().toOSString() );
        if( MessageDialog.openQuestion( parent, UITexts.scionServerStartupError_title, text ) ) {
          PreferenceDialog prefDialog = PreferencesUtil.createPreferenceDialogOn( parent, ScionPP.PAGE_ID, null, null );
          prefDialog.open();
        }
      }
    } );
  }

  /** Create a console name string using the project name, if available.
   * @param project The project
   * @return A console name
   */
  private final String consoleName ( final IProject project ) {
    String projectName = project != null ? project.getName() : UITexts.noproject;
    return NLS.bind( UITexts.scion_console_title, projectName );
  }

  /** Spawn a built-in server build job */
  synchronized void spawnBuildJob() {
    if (internalBuilder == null) {
      IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
      IOConsole console = new IOConsole(UITexts.scionServerBuildJob, null);
      internalBuilder = new ScionBuildJob(UITexts.scionServerBuildJob, console);

      mgr.addConsoles(new IConsole[] {console});
      mgr.showConsoleView( console );
      internalBuilder.setPriority( Job.BUILD );
      // Important to ensure that we schedule ourselves as a workspace job to prevent
      // other jobs from running (not that they can, but...)
      internalBuilder.setRule( ResourcesPlugin.getWorkspace().getRoot() );
      // This is a user visible task, so set the annoy bit:
      internalBuilder.setUser(true);
      internalBuilder.schedule();
    }
  }

  /** Spawn a built-in server build job */
  synchronized void spawnBrowserBuildJob() {
    if (internalBrowserBuilder == null) {
      IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
      IOConsole console = new IOConsole(UITexts.browserServerBuildJob, null);
      internalBrowserBuilder = new BrowserBuildJob(UITexts.browserServerBuildJob, console);

      mgr.addConsoles(new IConsole[] {console});
      mgr.showConsoleView( console );
      internalBrowserBuilder.setPriority( Job.BUILD );
      // Important to ensure that we schedule ourselves as a workspace job to prevent
      // other jobs from running (not that they can, but...)
      internalBrowserBuilder.setRule( ResourcesPlugin.getWorkspace().getRoot() );
      // This is a user visible task, so set the annoy bit:
      internalBrowserBuilder.setUser(true);
      internalBrowserBuilder.schedule();
    }
  }

  /**
   * Get the Haskell console for a project, creating one if necessary.
   *
   * @param consoleName The console's name
   */
  private HaskellConsole getHaskellConsole(final IProject project) {
    final String consoleName = consoleName(project);
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

  /** Specialized Job class that manages building the built-in Scion server,
   * providing some feedback to the user as the build progresses.
   *
    * @author B. Scott Michel
   */
  public class ScionBuildJob extends Job {
    ScionBuildStatus status;
    IOConsole fConsole;
    IOConsoleOutputStream fConOut;

    public ScionBuildJob(final String jobTitle, final IOConsole console) {
      super(jobTitle);
      status = new ScionBuildStatus();
      fConsole = console;
      fConOut = console.newOutputStream();

      console.activate();
      console.clearConsole();

      // If the build failed, there will be some indication of why it failed in the
      // ScionBuildStatus object. This is where we get to present that back to the
      // user:
      addJobChangeListener( new JobChangeAdapter() {
        @Override
        public void done( final IJobChangeEvent event ) {
          if (!event.getResult().isOK()) {
            Display.getDefault().syncExec( new Runnable() {
              public void run() {
                MessageDialog.openError( Display.getDefault().getActiveShell(),
                                         status.getTitle(),
                                         status.getMessage() );
              }
            } );
          } else {
            /*  Yippee! The server built successfully: Tell user and delete the console.
            Display.getDefault().syncExec( new Runnable() {
              public void run() {
                MessageDialog.openInformation( Display.getDefault().getActiveShell(),
                                               UITexts.scionServerProgress_completed_title,
                                               UITexts.scionServerProgress_completed_message );
              }
            } ); */

            // Dispose of the console on a successful build
            IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
            mgr.removeConsoles( new IConsole[] { fConsole } );
          }
          // reset builder even on failure otherwise it never launches again
          ScionManager.this.internalBuilder = null;
          super.done( event );
        }
      });
    }

    @Override
    protected IStatus run( final IProgressMonitor monitor ) {
      monitor.beginTask( UITexts.scionServerProgress_title, IProgressMonitor.UNKNOWN );
      status = buildBuiltIn(monitor, fConOut);
      monitor.done();

      if (status.isOK()) {
        synchronized (ScionManager.this) {
          ScionPlugin.shutdownAllInstances();
          ScionManager.this.serverFactorySetup();
        }
      }
      return status.getStatus();
    }
  }

  /** Specialized Job class that manages building the built-in Browser server,
   * providing some feedback to the user as the build progresses.
   *
    * @author B. Scott Michel
   */
  public class BrowserBuildJob extends Job {
    ScionBuildStatus status;
    IOConsole fConsole;
    IOConsoleOutputStream fConOut;

    public BrowserBuildJob(final String jobTitle, final IOConsole console) {
      super(jobTitle);
      status = new ScionBuildStatus();
      fConsole = console;
      fConOut = console.newOutputStream();

      console.activate();
      console.clearConsole();

      // If the build failed, there will be some indication of why it failed in the
      // ScionBuildStatus object. This is where we get to present that back to the
      // user:
      addJobChangeListener( new JobChangeAdapter() {
        @Override
        public void done( final IJobChangeEvent event ) {
          if (!event.getResult().isOK()) {
            Display.getDefault().syncExec( new Runnable() {
              public void run() {
                MessageDialog.openError( Display.getDefault().getActiveShell(),
                                         status.getTitle(),
                                         status.getMessage() );
              }
            } );
          } else {
            /*  Yippee! The server built successfully: Tell user and delete the console.
            Display.getDefault().syncExec( new Runnable() {
              public void run() {
                MessageDialog.openInformation( Display.getDefault().getActiveShell(),
                                               UITexts.scionServerProgress_completed_title,
                                               UITexts.scionServerProgress_completed_message );
              }
            } ); */

            // Dispose of the console on a successful build
            IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
            mgr.removeConsoles( new IConsole[] { fConsole } );
          }
          // reset builder even on failure otherwise it never launches again
          ScionManager.this.internalBrowserBuilder = null;
          super.done( event );
        }
      });
    }

    @Override
    protected IStatus run( final IProgressMonitor monitor ) {
      monitor.beginTask( UITexts.scionServerProgress_title, IProgressMonitor.UNKNOWN );
      status = buildBrowserBuiltIn(monitor, fConOut);
      monitor.done();

      if (status.isOK()) {
        synchronized (ScionManager.this) {
          BrowserPlugin.getSharedInstance().stop();
          ScionManager.this.browserSetup();
        }
      }
      return status.getStatus();
    }
  }

  public void processScionServerEvent( final ScionEvent ev ) {
    ScionEventType evType = ev.getEventType();
    final ScionInstance instance = (ScionInstance) ev.getSource();
    final Display display = Display.getDefault();

    if ( evType == ScionEventType.ABNORMAL_TERMINATION ) {
      // Ask the user if they'd like the server to be restarted.
      display.asyncExec( new Runnable() {
        public void run() {
          final String projectName = instance.getProject().getName();
          final String msg = NLS.bind( UITexts.scionServerAbnormalTermination_message, projectName );
          if ( MessageDialog.openQuestion( display.getActiveShell(), UITexts.scionServerAbnormalTermination_title, msg ) ) {
            ScionInstance instance = (ScionInstance) ev.getSource();
            try {
              instance.start();
            } catch( ScionServerStartupException ex ) {
              reportServerStartupError( ex );
            }
          }
        }
      } );
    } else if ( evType == ScionEventType.PROTOCOL_VERSION_MISMATCH ) {
      display.asyncExec( new Runnable() {
        public void run() {
          MessageDialog.openWarning( display.getActiveShell(), UITexts.scionVersionMismatch_title,
                                     UITexts.scionVersionMismatch_message );
        }
      } );
    }
  }

  /** Specialized Job class that manages loading the Browser databases.
   *  Based in the work of B. Scott Michel.
   *
    * @author B. Alejandro Serrano
   */
  public class BrowserDatabaseLoadJob extends Job {
    IStatus status;

    public BrowserDatabaseLoadJob(final String jobTitle) {
      super(jobTitle);
    }

    @Override
    protected IStatus run( final IProgressMonitor monitor ) {
      monitor.beginTask( UITexts.scionBrowserLoadingDatabases, IProgressMonitor.UNKNOWN );
      status = BrowserPlugin.loadLocalDatabase( false );
      monitor.done();

      return status;
    }
  }

  /** Specialized Job class that manages rebuilding the Browser database.
   *  Based in the work of B. Scott Michel.
   *
    * @author B. Alejandro Serrano
   */
  public class BrowserDatabaseRebuildJob extends Job {
    IStatus status;

    public BrowserDatabaseRebuildJob(final String jobTitle) {
      super(jobTitle);

      // If the build failed, there will be some indication of why it failed.
      addJobChangeListener( new JobChangeAdapter() {
        @Override
        public void done( final IJobChangeEvent event ) {
          if (event.getResult().isOK()) {
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
      status = BrowserPlugin.loadLocalDatabase( true );
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
