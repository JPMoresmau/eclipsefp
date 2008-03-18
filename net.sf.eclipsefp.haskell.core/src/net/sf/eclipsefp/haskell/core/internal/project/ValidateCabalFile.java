// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

import java.util.List;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.provisionary.MarkerDesc;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import de.leiffrenzel.cohatoe.server.core.CohatoeException;
import de.leiffrenzel.cohatoe.server.core.CohatoeServer;

public class ValidateCabalFile implements IValidateCabalFile {


  // interface methods of IValidateCabalFile
  //////////////////////////////////////////

  public void validate( final IFile cabalFile ) throws CohatoeException {
    String[] params = new String[] { cabalFile.getLocation().toOSString() };
    CohatoeServer server = CohatoeServer.getInstance();
    String[] retVal = server.evaluate( IValidateCabalFile.class, params );

    if( retVal != null ) {
      List<MarkerDesc> mds = MarkerDesc.unmarshal( retVal );
      for( MarkerDesc md: mds ) {
        try {
          md.setMarkerType( HaskellCorePlugin.ID_PROJECT_PROBLEM_MARKER );
          md.applyToResource();
        } catch( final CoreException cex ) {
          HaskellCorePlugin.log( cex );
        }
      }
    }
  }
}
