// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.scion;

import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.ui.util.CabalFileChangeListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.jobs.Job;

/**
 * <p>Reload project when Cabal file changes</p>
  *
  * @author JP Moresmau
 */
public class ProjectReloader implements CabalFileChangeListener{
  public void cabalFileChanged( final IFile cabalF ) {
    final ScionInstance si = ScionPlugin.getScionInstance( cabalF );
    if (si != null) {
      Job projectJob = si.buildProject( false, true );
      if (projectJob != null) {
        projectJob.schedule();
      }
    }
  }
}