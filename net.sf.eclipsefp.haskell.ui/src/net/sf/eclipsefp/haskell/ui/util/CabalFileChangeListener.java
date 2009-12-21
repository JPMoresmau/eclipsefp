package net.sf.eclipsefp.haskell.ui.util;

import org.eclipse.core.resources.IFile;

/**
 * <p>detects if the Cabal file of the project has been changed</p>
  *
  * @author JP Moresmau
 */
public interface CabalFileChangeListener {


  /*public void resourceChanged( final IResourceChangeEvent event ) {
    try {
      event.getDelta().accept( new IResourceDeltaVisitor() {

        public boolean visit( final IResourceDelta delta ) {
          if( delta.getKind() == IResourceDelta.CHANGED ) {
            if( delta.getResource() instanceof IFile ) {
              IFile f = ( IFile )delta.getResource();
              IFile cabalF = ScionInstance.getCabalFile( f.getProject() );
              if( f.equals( cabalF ) ) {
                cabalFileChanged(cabalF);
              }
              return false;
            }
          }
          return true;

        }
      } );

    } catch( CoreException ex ) {
      HaskellUIPlugin.log( UITexts.scion_delta_error, ex );
    }
  }*/

  void cabalFileChanged(IFile cabalF);
}