// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.scion;

import net.sf.eclipsefp.haskell.scion.client.IScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionInstanceFactory;
import net.sf.eclipsefp.haskell.ui.util.CabalFileChangeListener;
import org.eclipse.core.resources.IFile;

/**
 * <p>Reload project when Cabal file changes</p>
  *
  * @author JP Moresmau
 */
public class ProjectReloader implements CabalFileChangeListener{


  public void cabalFileChanged( final IFile cabalF ) {
    IScionInstance si = ScionInstanceFactory.getFactory().getScionInstance( cabalF );
    if (si!=null){
      si.buildProject( false,true );
    }
  }
}