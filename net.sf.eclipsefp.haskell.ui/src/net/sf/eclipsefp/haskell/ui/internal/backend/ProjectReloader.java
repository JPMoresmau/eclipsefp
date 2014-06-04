// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.backend;

import net.sf.eclipsefp.haskell.buildwrapper.BuildWrapperPlugin;
import net.sf.eclipsefp.haskell.buildwrapper.JobFacade;
import net.sf.eclipsefp.haskell.buildwrapper.types.BuildOptions;
import net.sf.eclipsefp.haskell.ui.util.CabalFileChangeListener;
import org.eclipse.core.resources.IFile;

/**
 * <p>Reload project when Cabal file changes</p>
  *
  * @author JP Moresmau
 */
@Deprecated
public class ProjectReloader implements CabalFileChangeListener {
  @Override
  public void cabalFileChanged( final IFile cabalF ) {
//    final ScionInstance si = ScionPlugin.getScionInstance( cabalF );
//
//    if (si!=null){
//      // setConfigure(true) is not needed since the cabal file will be more recent, but hey
//      BuildOptions buildOptions=new BuildOptions().setOutput(false).setRecompile(true).setConfigure( true );
//      si.buildProject(buildOptions );
//
//
//    }
    BuildWrapperPlugin.deleteProblems( cabalF );
    JobFacade f=BuildWrapperPlugin.getJobFacade( cabalF.getProject() );
    if (f!=null){
      f.synchronize(false);
      f.build( new BuildOptions().setOutput(false).setRecompile(true).setConfigure( true ) );
    }
  }
}