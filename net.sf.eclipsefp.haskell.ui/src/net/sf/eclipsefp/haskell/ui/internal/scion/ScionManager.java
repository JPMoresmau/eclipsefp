package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.io.File;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
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
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.scion.client.CabalComponentResolver;
import net.sf.eclipsefp.haskell.scion.client.IScionInstance;
import net.sf.eclipsefp.haskell.scion.client.IScionServerEventListener;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionInstanceFactory;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.client.ScionServerEvent;
import net.sf.eclipsefp.haskell.scion.client.ScionServerEventType;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ScionPP;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.CabalFileChangeListener;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
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
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.IPreferenceChangeListener;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.NodeChangeEvent;
import org.eclipse.core.runtime.preferences.IEclipsePreferences.PreferenceChangeEvent;
import org.eclipse.core.runtime.preferences.InstanceScope;
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
public class ScionManager implements IResourceChangeListener,ISchedulingRule {
  private String serverExecutable = null;

  /** Default constructor.
   *
   * @note Startup work is really done in the {@link #start start} method.
   */
  public ScionManager() {
    // the work is done in the start() method
  }

  /** Start the scion-server manager.
   *
   * This method checks the basic prerequisites to launching scion-server (or rebuilding scion-server when using the built-in),
   * and installs various event listeners that could change whether scion-server needs to be relaunched.
   */
  public void start() {
    IWorkspace workSpace = ResourcesPlugin.getWorkspace();
    IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    boolean useBuiltIn = preferenceStore.getBoolean( IPreferenceConstants.SCION_SERVER_BUILTIN );

    if (useBuiltIn) {
      if (   CompilerManager.getInstance().getCurrentHsImplementation() == null
          || CabalImplementationManager.getInstance().getDefaultCabalImplementation() == null) {
        // Either the default Haskell or Cabal implementations are unset, so wait for the preferences
        // to change:
        IEclipsePreferences hsCorePrefs = new InstanceScope().getNode( HaskellCorePlugin.getPluginId() );
        hsCorePrefs.addPreferenceChangeListener( new PreferenceStoreChangeListener() );
      } else if ( ScionBuilder.needsBuilding() ) {
        spawnBuildJob();
      }
      // Things look good, so set the scion instance factory's configuration
      scionInstanceFactory.setConfiguration(useBuiltIn, serverExecutablePath);
    }

    // creates the unattached instance used for lexing
    if (serverExecutable != null) {
      ScionInstance instance = startInstance( null );
      instances.put( null, instance );
    }

    preferenceStore.addPropertyChangeListener( new ScionServerPropertiesListener() );

    try {
      workSpace.getRoot().accept( new UpdateResourceVisitor() );
    } catch( CoreException ex ) {
      HaskellUIPlugin.log( UITexts.scion_delta_error, ex );
    }

    workSpace.addResourceChangeListener( this, IResourceChangeEvent.POST_CHANGE );
    workSpace.addResourceChangeListener( new FileDeletionListener(), IResourceChangeEvent.PRE_BUILD );
    workSpace.addResourceChangeListener( new CabalFileResourceChangeListener(), IResourceChangeEvent.POST_CHANGE );
    workSpace.addResourceChangeListener( new ProjectDeletionListener(), IResourceChangeEvent.PRE_DELETE);
  }

  public void stop() {
    ResourcesPlugin.getWorkspace().removeResourceChangeListener( this );
    scionInstanceFactory.stopAllInstances();
  }

  /** Check preference prerequisites and yell at the user if they're not satisfied
   *
   * (a) if using built-in server, do we have a haskell compiler and cabal to build it?
   * (b) if not using built-in server, is the server path set to something?
   *
   * @param useBuiltIn: Current preference value for "Use built-in Scion Server?"
   * @param serverExecutable: Current preference value for Scion-server executable path, if not using the built-in.
   */

  private boolean prerequisitesSatisfied( final boolean useBuiltIn, final String serverExecutable ) {
    String errMsg = new String();
    boolean retval = false;

    if ( useBuiltIn ) {
      if ( CompilerManager.getInstance().getCurrentHsImplementation() == null ) {
        errMsg = errMsg.concat( UITexts.prefsNoDefaultHaskellImpl );
      }
      if ( CabalImplementationManager.getInstance().getDefaultCabalImplementation() == null ) {
        errMsg = ((errMsg.length() > 0) ? errMsg.concat( PlatformUtil.NL ) : errMsg).concat( UITexts.prefsNoDefaultCabalImpl );
      }
    } else {
      if ( serverExecutable.length() > 0 ) {
        errMsg = ((errMsg.length() > 0) ? errMsg.concat( PlatformUtil.NL ) : errMsg).concat( UITexts.prefsEmptyServerName );
      }
    }

    if (errMsg.length() > 0) {
      final String theError = errMsg;
      Display.getDefault().syncExec( new Runnable() {
        public void run() {
          String theMessage = new String(UITexts.prefsPrerequisites_msg);
          theMessage = theMessage.concat( PlatformUtil.NL ).concat( PlatformUtil.NL ).concat(theError);
          MessageDialog.openError( Display.getDefault().getActiveShell(), UITexts.prefsPrerequisites_title, theMessage);
        }
      } );
    } else {
      retval = true;
    }

    return retval;
  }

  /**
   * Detect when a file is deleted and updates the Cabal file accordingly (remove the module). If it is the cabal file,
   * stop the scion server.
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
                        RealValuePosition rvp = pds.removeFromPropertyList(
                            CabalSyntax.FIELD_EXPOSED_MODULES, qn );
                        if (rvp!=null){
                          rvp.updateDocument( doc );
                          pd=PackageDescriptionLoader.load( doc.get() );
                          pds=pd.getSameStanza(pds);
                        }
                        rvp = pds.removeFromPropertyList(
                            CabalSyntax.FIELD_OTHER_MODULES, qn );
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
                  IScionInstance si = scionInstanceFactory.getScionInstance( f );
                  if (si!=null){
                    si.buildProject( false , true);
                  }
                  return false;
                } else if (f.equals( cabalF )){
                    scionInstanceFactory.stopInstance( f);
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
  public class ProjectDeletionListener implements IResourceChangeListener {
    public void resourceChanged( final IResourceChangeEvent event ) {
      if (event.getResource() instanceof IProject){
        scionInstanceFactory.stopInstance( event.getResource() );
      }
    }
  }

  /** Listener class that tracks changes to the Scion server properties */
  public class ScionServerPropertiesListener implements IPropertyChangeListener {
    public void propertyChange( final PropertyChangeEvent event ) {
      IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
      // By default, pick up what's in the preference store now.
      boolean useBuiltIn = preferenceStore.getBoolean( IPreferenceConstants.SCION_SERVER_BUILTIN );
      String serverExecutable = preferenceStore.getString( IPreferenceConstants.SCION_SERVER_EXECUTABLE ).trim();
      IPath serverExecutablePath = null;

      // built-in state has changed
      if (event.getProperty().equals( IPreferenceConstants.SCION_SERVER_BUILTIN )) {
        if (event.getNewValue() instanceof Boolean) {
          useBuiltIn = ((Boolean)event.getNewValue()).booleanValue();
        }
      }
      // Server executable path has changed.
      if( event.getProperty().equals( IPreferenceConstants.SCION_SERVER_EXECUTABLE ) ) {
        if(event.getNewValue() instanceof String ) {
          serverExecutable = ( String )event.getNewValue();
        }
      }

      // Convert to an IPath
      if (serverExecutable != null && serverExecutable.trim().length() > 0) {
        serverExecutablePath = new Path(serverExecutable.trim());
      }

      // Update the factory's configuration
      scionInstanceFactory.setConfiguration( useBuiltIn, serverExecutablePath );
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

  /** */
  public class PreferenceStoreChangeListener implements IPreferenceChangeListener {
    public void preferenceChange( final PreferenceChangeEvent event ) {
      String key = event.getKey();
      HaskellUIPlugin.log( "Core pref change: ".concat(event.getSource().toString()), IStatus.INFO );
      HaskellUIPlugin.log( "Core pref changed key: ".concat(key), IStatus.INFO );

      if(    ICorePreferenceNames.HS_IMPLEMENTATIONS.equals( key )
          || ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION.equals( key ) ) {
        // spawnBuildJob();
      }
    }
  }

  /** Listen for changes to nodes in HaskellCorePlugin's hierarchical preferences. */
  public class CorePreferencesNodeChangeListener implements INodeChangeListener {
    public void added( final NodeChangeEvent event ) {
      HaskellUIPlugin.log( "Added node: ".concat(event.getSource().toString()), IStatus.INFO );
      HaskellUIPlugin.log( "-- child node: ".concat( event.getChild().name() ), IStatus.INFO );
    }

    public void removed( final NodeChangeEvent event ) {
      // unused.
    }

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
      IProject project = ( IProject )resource;
      if(    project.isOpen()
          && !scionInstanceFactory.hasScionInstance( project )
          && project.hasNature( HaskellNature.NATURE_ID ) ) {
        startInstance( project );
      }
      if( !project.isOpen() && scionInstanceFactory.hasScionInstance( project ) ) {
        // we cannot check the nature of closed projects, but if it's in
        // instances, stop it
        scionInstanceFactory.stopInstance( project );
      }
      return true; // projects can't be children of other projects, can they?
    }
    return true;
  }

  /**
   * Starts and returns a new Scion instance for the given project. Does not add
   * the instance to the instances map.
   */
  private synchronized IScionInstance startInstance( final IProject project ) {
    IScionInstance instance = scionInstanceFactory.createScionServerInstance( project );
    try {
      instance.start();
    } catch( ScionServerStartupException ex ) {
      reportServerStartupError( ex );
    }
    return instance;
  }

  private void reportServerStartupError( final ScionServerStartupException ex ) {
    if( !serverStartupErrorReported ) {
      IStatus status = new Status( IStatus.ERROR,
          HaskellUIPlugin.getPluginId(), ex.getMessage(), ex );
      StatusManager.getManager().handle( status, StatusManager.LOG );
      HaskellUIPlugin.getStandardDisplay().asyncExec( new Runnable() {

        public void run() {
          Shell parent = HaskellUIPlugin.getStandardDisplay().getActiveShell();
          String text = NLS.bind( UITexts.scionServerStartupError_message,
              ScionPP.getServerExecutableName() );
          if( MessageDialog.openQuestion( parent,
              UITexts.scionServerStartupError_title, text ) ) {
            PreferenceDialog prefDialog = PreferencesUtil
                .createPreferenceDialogOn( parent, ScionPP.PAGE_ID, null, null );
            prefDialog.open();
          }
        }
      } );
      serverStartupErrorReported = true;
    }
  }

  /** Process an event sent by ScionInstanceFactory */
  public void processScionServerEvent( final ScionServerEvent ev ) {
    ScionServerEventType evType = ev.getEventType();
    if( evType == ScionServerEventType.NEEDS_REBUILD ) {
      buildBuiltIn();
    } else if( evType == ScionServerEventType.VALID_RUNNING ) {
      ScionInstance instance = ( ScionInstance )ev.getSource();
      if( instance != null ) {
        instance.setComponentResolver( new CabalComponentResolver() {
          public Set<String> getComponents( final IFile file ) {
            Set<PackageDescriptionStanza> pds = ResourceUtil.getApplicableStanzas( new IFile[] { file } );
            Set<String> ret = new HashSet<String>( pds.size() );
            for( PackageDescriptionStanza pd: pds ) {
              ret.add( pd.toTypeName() );
            }
            return ret;
          }
        } );
      }
    }
  }

  /** Spawn a built-in server build job */
  private void buildBuiltIn() {
    IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
    IOConsole console = new IOConsole(UITexts.scionServerBuildJob, null);
    Job job = new ScionBuildJob(UITexts.scionServerBuildJob, console);

    mgr.addConsoles(new IConsole[] {console});
    mgr.showConsoleView( console );
    job.setRule( this );
    job.setPriority( Job.BUILD );
    job.schedule();
  }

  /** Specialized Job class that manages building the internal Scion server,
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
            // Build was successful, so we don't need the console any more.
            IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
            mgr.removeConsoles( new IConsole[] { fConsole } );
          }
          super.done( event );
        }
      });
    }

    private ScionBuildStatus buildBuiltIn(final IProgressMonitor monitor, final IOConsoleOutputStream conout) {
      IPath scionBuildDirPath = ScionPlugin.builtinServerDirectoryPath();
      File scionBuildDir = scionBuildDirPath.toFile();
      ScionBuildStatus retval;

      monitor.subTask( "Unpacking the Scion server source archive" );
      retval = ScionBuilder.unpackScionArchive( scionBuildDir );
      if (retval.isOK()) {
        // build final executable location
        CabalImplementationManager cabalMgr = CabalImplementationManager.getInstance();
        CabalImplementation cabalImpl = cabalMgr.getDefaultCabalImplementation();

        IPath exePath = ScionPlugin.serverExecutablePath( scionBuildDirPath );
        File  exeFile = exePath.toFile();

        if( !exeFile.exists() && cabalImpl != null) {
          monitor.subTask( "Cabal building the internal Scion server" );
          retval = ScionBuilder.build( cabalImpl, scionBuildDir, conout);
          if (retval.isOK() && exeFile.exists() ) {
            retval.setExecutable( exePath );
          }
        } else {
          if (cabalImpl == null ) {
            retval.buildFailed(UITexts.noCabalImplementation_title, UITexts.noCabalImplementation_message);
          }
        }
      }
      return retval;
    }

    @Override
    protected IStatus run( final IProgressMonitor monitor ) {
      monitor.beginTask( UITexts.scionServerProgress_title, IProgressMonitor.UNKNOWN );
      status = buildBuiltIn(monitor, fConOut);
      if (status.isOK()) {
        scionInstanceFactory.serverExecutableChanged(status.getExecutable() );
      }
      monitor.done();
      return status.getStatus();
    }
  }
}
