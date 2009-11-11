package net.sf.eclipsefp.haskell.ui.internal.resolve;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator;

/**
 * <p>Provides resolutions for markers</p>
  *
  * @author JP Moresmau
 */
public class BuildMarkerResolutionGenerator implements
    IMarkerResolutionGenerator {

  public IMarkerResolution[] getResolutions( final IMarker marker ) {
    if (marker.getAttribute( IMarker.SEVERITY , IMarker.SEVERITY_ERROR)==IMarker.SEVERITY_WARNING){
      String msg=marker.getAttribute(IMarker.MESSAGE,""); //$NON-NLS-1$
      if (msg!=null && marker.getResource() instanceof IFile && msg.toLowerCase().indexOf( GhcMessages.WARNING_NOTYPE_CONTAINS )>-1){
          return new IMarkerResolution[]{new MissingTypeWarningResolution()};
      }
    }
    return new IMarkerResolution[0];
  }

}
