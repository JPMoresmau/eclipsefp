package net.sf.eclipsefp.haskell.ui.wizards.cabal;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;

/**
 * <p>Calls Cabal sdist</p>
  *
  * @author JP Moresmau
 */
public class CabalSDistWizard extends Wizard implements IExportWizard {
  private final Set<IProject> projects=new LinkedHashSet<IProject>();
  private CabalSDistOptionsPage optionsPage;

  public CabalSDistWizard() {
    setDialogSettings( HaskellUIPlugin.getDefault().getDialogSettings() );
    setWindowTitle( Platform.getResourceBundle( Platform.getBundle( HaskellUIPlugin.getPluginId() )).getString( "cabalSDistWizard.name" ));
  }

  @Override
  public void addPages() {
    optionsPage=new CabalSDistOptionsPage(projects) ;
    addPage(optionsPage );

  }


  @Override
  public boolean performFinish() {
    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    if (cabalExecutable!=null){
      final List<String> commands = new ArrayList<String>();
      commands.add( cabalExecutable );
      commands.add("sdist");
      // options
      commands.add("--builddir="+optionsPage.getFolder());

      if (optionsPage.isSnapshot()){
        commands.add( "--snapshot" );
      }

      final String fold= optionsPage.getFolder();
      for (final IProject p:projects){
        Runnable r=new Runnable() {

          public void run() {
            IFolder f=p.getFolder(fold );
            if (f!=null){
              // refresh folder
              try {
                f.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
              } catch ( CoreException ce ) {
                HaskellUIPlugin.log( ce );
              }
            }
          }
        };
        try {
          AbstractHaskellLaunchDelegate.runInConsole(p, commands, new File(p.getLocation().toOSString()), NLS.bind( UITexts.exportSource_job, p.getName() ),false,r );
        } catch (Exception ioe){
          HaskellUIPlugin.log(ioe);
          final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);
          ErrorDialog.openError( getShell(), UITexts.exportSource_error, UITexts.exportSource_error_text, st);
        }
      }
//        new Job(NLS.bind( UITexts.exportSource_job, p.getName() )) {
//
//          @Override
//          protected IStatus run(final IProgressMonitor arg0) {
//            StringWriter out=new StringWriter();
//            try {
//              int ret=new ProcessRunner().executeBlocking( new File(p.getLocation().toOSString()) , out, null, commands.toArray( new String[commands.size()]));
//              String output=out.toString();
//           // let's not rely on Eclipse to show us the status, display it ourselves and return ok
//              if (ret!=0 ){
//                HaskellUIPlugin.log(out.toString() ,IStatus.ERROR );
//                final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), out.toString());
//                Display.getDefault().asyncExec( new Runnable(){
//                  public void run() {
//                    ErrorDialog.openError( getShell(), UITexts.exportSource_error, UITexts.exportSource_error_text, st );
//                  }
//                } );
//              } else if (hasWarnings( output)){
//                HaskellUIPlugin.log(out.toString() ,IStatus.WARNING );
//                final IStatus st= new Status( IStatus.WARNING, HaskellUIPlugin.getPluginId(), out.toString());
//                Display.getDefault().asyncExec( new Runnable(){
//                  public void run() {
//                    ErrorDialog.openError( getShell(), UITexts.exportSource_warning, UITexts.exportSource_warning_text, st );
//                  }
//                } );
//              }
//
//
//              return Status.OK_STATUS;
//            } catch (IOException ioe){
//              HaskellUIPlugin.log(ioe);
//              return new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), UITexts.exportSource_error, ioe );
//            } finally {
//              arg0.done();
//            }
//          }
//        }.schedule();
//      }
    }
    return true;
  }

  /**
   * see if output contains any of the marker for warnings. Not performance critical
   */
//  private static boolean hasWarnings(final String output){
//    if (output!=null && output.length()>0){
//      String lc=output.toLowerCase();
//      String markers=UITexts.exportSource_warning_markers;
//      StringTokenizer st=new StringTokenizer( markers, " " );
//      while (st.hasMoreTokens()){
//        if (lc.contains( st.nextToken() )){
//          return true;
//        }
//      }
//    }
//    return false;
//  }

  public void init( final IWorkbench arg0, final IStructuredSelection arg1 ) {
    projects.addAll( ResourceUtil.getProjects( arg1 ) );

  }

}
