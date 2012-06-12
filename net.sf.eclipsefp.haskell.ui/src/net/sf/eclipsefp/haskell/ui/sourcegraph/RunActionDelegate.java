/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.sourcegraph;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.browser.IWebBrowser;
import org.eclipse.ui.browser.IWorkbenchBrowserSupport;

/**
 * Action that runs SourceGraph and shows the resulting analysis
 * to the user in a browser page.
 * @author Alejandro Serrano
 *
 */
public class RunActionDelegate implements IObjectActionDelegate {
  private Shell currentShell;
  private IProject project;

  public static String SOURCEGRAPH = "SourceGraph";
  public static String HTML_EXTENSION = "html";

  public RunActionDelegate() {
    // Nothing special
  }

  @Override
  public void run( final IAction action ) {
    if (project != null) {
      try {
        final IFile cabalFile = BuildWrapperPlugin.getCabalFile( project );
        // Run the command
        final List<String> commands = new ArrayList<String>();
        commands.add( ScionManager.getExecutablePath( IPreferenceConstants.SOURCEGRAPH_EXECUTABLE,SOURCEGRAPH, false ) );
        commands.add(cabalFile.getRawLocation().toOSString());

        AbstractHaskellLaunchDelegate.runInConsole( project, commands, new File(project.getLocation().toOSString()), SOURCEGRAPH, false,new Runnable(){
          @Override
          public void run() {
         // Get path name
            final IPath projectPath = cabalFile.getRawLocation().removeLastSegments( 1 );
            final IPath htmlPath = projectPath.append( SOURCEGRAPH ).append( project.getName() ).addFileExtension( HTML_EXTENSION );
            final File htmlFile = htmlPath.toFile();

            Display.getDefault().syncExec( new Runnable() {

              @Override
              public void run() {
                try {
                  if( htmlFile.exists() && htmlFile.isFile() ) {
                    // Refresh workspace
                    project.refreshLocal( IResource.DEPTH_ONE, null );
                    // Open URL
                    IWorkbenchBrowserSupport browserSupport = PlatformUI.getWorkbench().getBrowserSupport();
                    URL url = htmlFile.toURI().toURL();
                    IWebBrowser browser = browserSupport.createBrowser( IWorkbenchBrowserSupport.AS_EDITOR, null, SOURCEGRAPH, "" );
                    browser.openURL(url);
                  } else {
                    // Do something if the file does not exist
                  }
                } catch( Exception e ) {
                 HaskellUIPlugin.log( e );
                }
              }
            } );

          }
        } );


      } catch (Exception e) {
        final IStatus st = new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), e.toString());
        ErrorDialog.openError( currentShell, UITexts.runSourceGraph_errorTitle, UITexts.runSourceGraph_error, st);
      }
    }
  }

  @Override
  public void selectionChanged( final IAction action, final ISelection selection ) {
    Collection<IProject> prjs = ResourceUtil.getProjects( selection );
    if (prjs.size() > 0){
      project = prjs.iterator().next();
    }
  }

  @Override
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart ) {
    currentShell = targetPart.getSite().getShell();
  }

}
