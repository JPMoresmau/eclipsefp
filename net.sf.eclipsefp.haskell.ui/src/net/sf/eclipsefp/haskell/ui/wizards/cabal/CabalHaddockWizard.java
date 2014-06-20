/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
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
import org.eclipse.core.resources.IFile;
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
import org.eclipse.ui.PlatformUI;


/**
 * this wizard run cabal haddock on all selected projects, with options
 * @author JP Moresmau
 *
 */
public class CabalHaddockWizard extends Wizard implements IExportWizard {
  private final Set<IProject> projects=new LinkedHashSet<>();
  private CabalHaddockOptionsPage optionsPage;

  /**
   *
   */
  public CabalHaddockWizard() {
    setDialogSettings( HaskellUIPlugin.getDefault().getDialogSettings() );
    setWindowTitle( Platform.getResourceBundle( Platform.getBundle( HaskellUIPlugin.getPluginId() )).getString( "cabalHaddockWizard.name" ));

  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench, org.eclipse.jface.viewers.IStructuredSelection)
   */
  @Override
  public void init( final IWorkbench arg0, final IStructuredSelection arg1 ) {
    projects.addAll( ResourceUtil.getProjects( arg1 ) );

  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  @Override
  public void addPages() {
    optionsPage=new CabalHaddockOptionsPage(projects);
    addPage(optionsPage);
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish() {
    final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
    if (cabalExecutable!=null){
      final List<String> commands = new ArrayList<>();
      commands.add( cabalExecutable );
      commands.add("haddock");
      // options
      commands.add("--builddir="+optionsPage.getFolder());

      if (CabalHaddockOptionsPage.isDoHoogle()){
        commands.add("--hoogle");
      }
      if (CabalHaddockOptionsPage.isDoExecutables()){
        commands.add("--executables");
      }
      if (CabalHaddockOptionsPage.isDoInternal()){
        commands.add("--internal");
      }
      if (CabalHaddockOptionsPage.isDoHtml() && CabalHaddockOptionsPage.getHtml().length()>0){
        commands.add("--html-location="+CabalHaddockOptionsPage.getHtml());
      }
      if (CabalHaddockOptionsPage.isDoCss() && CabalHaddockOptionsPage.getCss().length()>0){
        commands.add("--css="+CabalHaddockOptionsPage.getCss());
      }

      if (CabalHaddockOptionsPage.isDoColourSrc()){
        commands.add("--hyperlink-source");
        if (CabalHaddockOptionsPage.isDoColourSrcCss() && CabalHaddockOptionsPage.getColourSrcCss().length()>0){
          commands.add("--hscolour-css="+CabalHaddockOptionsPage.getColourSrcCss());
        }
      }
      final String fold= optionsPage.getFolder();
      for (final IProject p:projects){
        Runnable r=new Runnable() {

          @Override
          public void run() {
            IFolder f=p.getFolder(fold );
            if (f!=null){
              // refresh folder
              try {
                f.refreshLocal( IResource.DEPTH_INFINITE, new NullProgressMonitor() );
              } catch ( CoreException ce ) {
                HaskellUIPlugin.log( ce );
              }
              // open library doc first
              IFile idx=f.getFile("doc/html/"+ p.getName()+"/index.html" );
              if (idx!=null && idx.exists()){
                try {
                  PlatformUI.getWorkbench().getBrowserSupport().createBrowser(p.getName() ).openURL( idx.getLocationURI().toURL() );
                } catch ( Exception ce ) {
                  HaskellUIPlugin.log( ce );
                }
              }
            }
          }
        };
        try {
          AbstractHaskellLaunchDelegate.runInConsole(p, commands, new File(p.getLocation().toOSString()), NLS.bind( UITexts.exportDoc_job, p.getName() ),false ,r);
        } catch (Exception ioe){
          HaskellUIPlugin.log(ioe);
          final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);
          ErrorDialog.openError( getShell(), UITexts.exportDoc_error, UITexts.exportDoc_error_text, st);
        }
      }
    }
    return true;
  }

}
