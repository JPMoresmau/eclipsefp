/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.resolve;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.AbstractHaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.scion.CabalFileChangeListenerManager;
import net.sf.eclipsefp.haskell.ui.internal.scion.ScionManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.CabalFileChangeListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Display;


/**
 * Install missing package(s) via cabal
 * @author JP Moresmau
 *
 */
public class InstallMissingPackage extends MarkerCompletion {
  private Set<String> packages=new HashSet<String>();


  public InstallMissingPackage( final Set<String> packages ) {
    super();
    this.packages = packages;
  }

  /* (non-Javadoc)
   * @see org.eclipse.ui.IMarkerResolution#getLabel()
   */
  @Override
  public String getLabel() {
    if (packages.size()==1){
      return NLS.bind( UITexts.resolve_install_one,packages.iterator().next());
    } else {
      return UITexts.resolve_install_all;
    }
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.resolve.MarkerCompletion#getCompletionProposal(org.eclipse.core.resources.IMarker, org.eclipse.jface.text.IDocument)
   */
  @Override
  public ICompletionProposal getCompletionProposal( final IMarker marker,
      final IDocument document ) {
    return new ICompletionProposal() {

      @Override
      public Point getSelection( final IDocument arg0 ) {
        return null;
      }

      @Override
      public Image getImage() {
        return null;
      }

      @Override
      public String getDisplayString() {
       return getLabel();
      }

      @Override
      public IContextInformation getContextInformation() {
        return null;
      }

      @Override
      public String getAdditionalProposalInfo() {
        return null;
      }

      @Override
      public void apply( final IDocument arg0 ) {
        final String cabalExecutable=CabalImplementationManager.getCabalExecutable();
        if (cabalExecutable==null){
          HaskellUIPlugin.log( UITexts.noCabalImplementationForInstall_error, IStatus.ERROR );
          return;
        }
        final List<String> commands = new ArrayList<String>();
        commands.add( cabalExecutable );
        commands.add("install");
        ScionManager.addCabalInstallOptions( commands );
        for (String s:packages){
          commands.add("\""+s+"\"");
        }
        try {
          final IProject project=marker.getResource().getProject();
          AbstractHaskellLaunchDelegate.runInConsole( project, commands, new File(""), getLabel(), true , new Runnable(){
            /* (non-Javadoc)
             * @see java.lang.Runnable#run()
             */
            @Override
            public void run() {
              IFile cabalF = BuildWrapperPlugin.getCabalFile( project );

              BWFacade bwf=BuildWrapperPlugin.getFacade( project );
              if (bwf!=null){
                bwf.cabalFileChanged();
              }
              for (CabalFileChangeListener l:CabalFileChangeListenerManager.getListeners()){
                l.cabalFileChanged( cabalF );
              }

            }
          });
        } catch (Exception ioe){
          HaskellUIPlugin.log(ioe);
          if (Display.getCurrent()!=null){
            final IStatus st=new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(),ioe.getLocalizedMessage(),ioe);
            ErrorDialog.openError( Display.getCurrent().getActiveShell(), UITexts.install_error, UITexts.install_error_text, st);
          }
        }
      }
    };
  }

}
