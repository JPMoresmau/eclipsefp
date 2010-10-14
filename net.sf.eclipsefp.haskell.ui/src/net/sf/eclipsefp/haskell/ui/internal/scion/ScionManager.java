package net.sf.eclipsefp.haskell.ui.internal.scion;

import java.io.File;
import java.io.Writer;
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
import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
import net.sf.eclipsefp.haskell.core.preferences.ICorePreferenceNames;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.scion.client.CabalComponentResolver;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.scion.exceptions.ScionServerStartupException;
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
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
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
public class ScionManager implements IResourceChangeListener, ISchedulingRule {
  public ScionManager() {
    // the work is done in the start() method
  }

  public void start() {
    IWorkspace workSpace = ResourcesPlugin.getWorkspace();
    IPreferenceStore preferenceStore = HaskellUIPlugin.getDefault().getPreferenceStore();
    boolean useBuiltIn = preferenceStore.getBoolean( IPreferenceConstants.SCION_SERVER_BUILTIN );

    // Sit and listen to the core preference store changes
    HaskellCorePlugin.instanceScopedPreferences().addPreferenceChangeListener( new CorePreferencesChangeListener() );

    try {
      if (useBuiltIn) {
        if (   CompilerManager.getInstance().getCurrentHsImplementation() != null
            && CabalImplementationManager.getInstance().getDefaultCabalImplementation() != null) {
            if ( !ScionBuilder.needsBuilding() ) {
              // FIXME: Uses stdstream now, could use network pipe, depending on preferences?
              ScionPlugin.useBuiltInStdStreamServerFactory();
            } else {
              spawnBuildJob();
            }
        }
      } else {
        String serverExecutable = preferenceStore.getString( IPreferenceConstants.SCION_SERVER_EXECUTABLE );
        ScionPlugin.useStdStreamScionServerFactory( new Path( serverExecutable ) );
      }
    } catch (ScionServerStartupException ex) {
      reportServerStartupError( ex );
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

  private ScionBuildStatus buildBuiltIn(final IProgressMonitor monitor, final IOConsoleOutputStream conout) {
    IPath scionBuildDirPath = ScionPlugin.builtinServerDirectoryPath();
    File scionBuildDir = scionBuildDirPath.toFile();
    ScionBuildStatus retval;

    monitor.subTask( "Unpacking the Scion server source archive" );
    retval = ScionBuilder.unpackScionArchive( scionBuildDir );
    if (retval.isOK()) {
      // build final exe location
      IHsImplementation hsImpl = CompilerManager.getInstance().getCurrentHsImplementation();
      CabalImplementationManager cabalMgr = CabalImplementationManager.getInstance();
      CabalImplementation cabalImpl = cabalMgr.getDefaultCabalImplementation();

      IPath exePath = ScionPlugin.serverExecutablePath( scionBuildDirPath );
      File  exeFile = exePath.toFile();

      if( !exeFile.exists() && hsImpl != null && cabalImpl != null) {
        monitor.subTask( "Cabal building the internal Scion server" );
        retval = ScionBuilder.build( cabalImpl, scionBuildDir, conout);
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
   * <p>detects when a file is deleted and updates the Cabal file accordingly (remove the module). If it is the cabal file, stop scion</p>
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
                  ScionInstance si = ScionPlugin.getScionInstance( f );
                  if (si!=null){
                    si.buildProject( false , true);
                  }
                  return false;
                } else if (f.equals( cabalF )){
                    stopInstance( f);
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
              ScionPlugin.useStdStreamScionServerFactory( new Path(serverExecutable) );
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
  public class CorePreferencesChangeListener implements IPreferenceChangeListener {
    public void preferenceChange( final PreferenceChangeEvent event ) {
      String key = event.getKey();
      HaskellUIPlugin.log( "Property changed: ".concat(key), IStatus.INFO );
      if(    ICorePreferenceNames.HS_IMPLEMENTATIONS.equals( key )
          || ICorePreferenceNames.SELECTED_HS_IMPLEMENTATION.equals( key )
          || ICorePreferenceNames.CABAL_IMPLEMENTATIONS.equals( key ) ) {
        spawnBuildJob();
      }
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
      HaskellConsole c = new HaskellConsole( null, consoleName(project) );
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

  public boolean contains(final ISchedulingRule rule) {
    return rule == this;
  }

  public boolean isConflicting(final ISchedulingRule rule) {
    return rule == this;
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
  void spawnBuildJob() {
    IConsoleManager mgr = ConsolePlugin.getDefault().getConsoleManager();
    IOConsole console = new IOConsole(UITexts.scionServerBuildJob, null);
    Job job = new ScionBuildJob(UITexts.scionServerBuildJob, console);

    mgr.addConsoles(new IConsole[] {console});
    mgr.showConsoleView( console );
    job.setRule( ScionManager.this );
    job.setPriority( Job.BUILD );
    job.schedule();
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

    @Override
    protected IStatus run( final IProgressMonitor monitor ) {
      monitor.beginTask( UITexts.scionServerProgress_title, IProgressMonitor.UNKNOWN );
      status = buildBuiltIn(monitor, fConOut);
      if (status.isOK()) {
        try {
          ScionPlugin.useBuiltInStdStreamServerFactory();
        } catch (ScionServerStartupException ex) {
          // Should never happen, but who knows...
          reportServerStartupError( ex );
        }
      }
      monitor.done();
      return status.getStatus();
    }
  }
}
