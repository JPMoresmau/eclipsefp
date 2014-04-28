/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.actions;

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
import net.sf.eclipsefp.haskell.ui.views.CabalPackagesView;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;


/**
 * run cabal check
 * @author JP Moresmau
 *
 */
public class CabalCheckAction implements IObjectActionDelegate {
  private final Set<IProject> projects=new LinkedHashSet<IProject>();
  private Shell currentShell;


  @Override
  public void setActivePart( final IAction arg0, final IWorkbenchPart arg1 ) {
    currentShell=arg1.getSite().getShell();

  }


  @Override
  public void run( final IAction arg0 ) {

    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    if (cabalExecutable!=null){
      final List<String> commands = new ArrayList<String>();
      commands.add( cabalExecutable );
      commands.add("check");

      for (final IProject p:projects){
        try {
          List<String> prjCommands = new ArrayList<String>(commands);
          /*BWFacade bf=BuildWrapperPlugin.getFacade( p );
          // need to provide user supplied info
          if(bf!=null){
            String f=bf.getFlags();
            if (f!=null && f.length()>0){
              prjCommands.add("--flags="+f);
            }
            List<String> extraOpts=bf.getExtraOpts();
            if (extraOpts!=null){
              for (String eo:extraOpts){
                prjCommands.add(eo);
              }
            }
          }*/


          AbstractHaskellLaunchDelegate.runInConsole(p, prjCommands, new File(p.getLocation().toOSString()), NLS.bind( getJobName(), p.getName() ),true,getAfter(p) );
        } catch (Exception ioe){
          HaskellUIPlugin.log(ioe);
          final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);
          ErrorDialog.openError( currentShell, UITexts.check_error, UITexts.check_error_text, st);
        }


      }
    }


  }

  protected Runnable getAfter(final IProject p){
    return new Runnable() {

      @Override
      public void run() {
        /** refresh the cabal packages view **/
        CabalPackagesView.refresh();
      }
    };
  }

  protected String getJobName(){
    return UITexts.install_job;
  }

  protected String getSandboxWarningMessage(){
    return UITexts.install_sandbox_install_text;
  }

  protected void addExtraParameters(final List<String> commands){
    // force reinstall since we're probably reinstalling our development version
    commands.add( "--reinstall" );
  }

  @Override
  public void selectionChanged( final IAction arg0, final ISelection arg1 ) {
    projects.clear();
    projects.addAll( ResourceUtil.getProjects( arg1 ) );
  }

}
