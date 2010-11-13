package net.sf.eclipsefp.haskell.ui.wizards;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementation;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.util.ProcessRunner;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.progress.UIJob;

/**
 * <p>Calls Cabal sdist</p>
  *
  * @author JP Moresmau
 */
public class CabalSDistWizard extends Wizard implements IExportWizard {
  private final List<IProject> projects=new LinkedList<IProject>();
  private CabalSDistOptionsPage optionsPage;

  public CabalSDistWizard() {
    setDialogSettings( HaskellUIPlugin.getDefault().getDialogSettings() );
  }

  @Override
  public void addPages() {
    optionsPage=new CabalSDistOptionsPage(projects) ;
    addPage(optionsPage );
  }

  @Override
  public boolean performFinish() {
    CabalImplementationManager cabalMgr = CabalImplementationManager.getInstance();
    CabalImplementation cabalImpl = cabalMgr.getDefaultCabalImplementation();
    if (cabalImpl!=null){
      final String cabalExecutable = cabalImpl.getCabalExecutableName().toOSString();
      if (cabalExecutable==null || cabalExecutable.length()<1){
        HaskellUIPlugin.log( UITexts.zerolenCabalExecutable_message,IStatus.ERROR);
        return false;
      }
      final List<String> commands = new ArrayList<String>();
      commands.add( cabalExecutable );
      commands.add("sdist");
      // options
      commands.add("--builddir="+optionsPage.getFolder());

      if (optionsPage.isSnapshot()){
        commands.add( "--snapshot" );
      }

      for (final IProject p:projects){
        new UIJob(NLS.bind( UITexts.exportSource_job, p.getName() )) {

          @Override
          public IStatus runInUIThread( final IProgressMonitor arg0 ) {
            StringWriter out=new StringWriter();
            try {
              int ret=new ProcessRunner().executeBlocking( new File(p.getLocation().toOSString()) , out, null, commands.toArray( new String[commands.size()]));
              if (ret!=0){
                HaskellUIPlugin.log(out.toString() ,IStatus.ERROR );
                return new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), UITexts.exportSource_error);
              }
              return Status.OK_STATUS;
            } catch (IOException ioe){
              HaskellUIPlugin.log(ioe);
              return new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), UITexts.exportSource_error, ioe );
            } finally {
              arg0.done();
            }
          }
        }.schedule();
        return true;
      }
    }
    return false;
  }

  public void init( final IWorkbench arg0, final IStructuredSelection arg1 ) {
    for (Iterator<?> it=arg1.iterator();it.hasNext();){
      IResource res = ResourceUtil.findResource( it.next() );
      if( res != null && res instanceof IProject ) {
        projects.add( (IProject) res );
      }
    }

  }

}
