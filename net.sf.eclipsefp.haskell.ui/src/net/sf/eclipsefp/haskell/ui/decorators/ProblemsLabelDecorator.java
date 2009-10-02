package net.sf.eclipsefp.haskell.ui.decorators;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;

/**
 * LabelDecorator that decorates an element's image with error and warning
 * overlays that represent the severity of markers attached to the element's
 * underlying resource. To see a problem decoration for a marker, the marker
 * needs to be a subtype of <code>IMarker.PROBLEM</code>.
 *
 * @author Thomas ten Cate
 */
public class ProblemsLabelDecorator implements ILightweightLabelDecorator {

  private final ListenerList listeners = new ListenerList();

  public void decorate( final Object element, final IDecoration decoration ) {
    int adornmentFlags = computeAdornmentFlags( element );
    ImageDescriptor desc;
    switch (adornmentFlags) {
      case IMarker.SEVERITY_ERROR:
        desc = getImageDescriptor( IImageNames.ERROR_OVERLAY );
        break;
      case IMarker.SEVERITY_WARNING:
        desc = getImageDescriptor( IImageNames.WARNING_OVERLAY );
        break;
      default:
        desc = null; // unchanged
        break;
    }
    if (desc != null) {
      decoration.addOverlay( desc, IDecoration.BOTTOM_LEFT );
    }
  }

  protected int computeAdornmentFlags( final Object obj ) {
    if (obj instanceof IResource && !((IResource)obj).exists()){
      return 0;
    }
    try {
      if( obj instanceof IFile
          && ResourceUtil.hasHaskellExtension( ( IResource )obj ) ) {
        return ( ( IResource )obj ).findMaxProblemSeverity( IMarker.PROBLEM,
            true, IResource.DEPTH_ZERO );
      } else if( obj instanceof IFolder
          && ResourceUtil.isSourceFolder( ( IFolder )obj ) ) {
        return ( ( IResource )obj ).findMaxProblemSeverity( IMarker.PROBLEM,
            true, IResource.DEPTH_INFINITE );
      }
    } catch( CoreException e ) {
      HaskellUIPlugin.log( e );
    }
    return 0;
  }

  private ImageDescriptor getImageDescriptor(final String name) {
    return HaskellUIImages.getImageDescriptor( name );
  }

  public boolean isLabelProperty( final Object element, final String property ) {
    // We currently only support labelling (some kinds of) resources
    return element instanceof IResource;
  }

  public void dispose() {
    // nothing to dispose
  }

  public void removeListener( final ILabelProviderListener listener ) {
    listeners.remove(listener);
  }

  public void addListener( final ILabelProviderListener listener ) {
    listeners.add(listener);
  }

}
