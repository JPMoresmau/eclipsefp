package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model;

import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;
import org.eclipse.core.runtime.IPath;


public abstract class GHCPackageResource implements ITreeElement {

  private final Object parent;
  protected final IPath location;

  public GHCPackageResource( final Object parent, final IPath location ) {
    if( parent == null || location == null ) {
      throw new IllegalArgumentException();
    }
    this.parent = parent;
    this.location = location;
  }


  // interface methods of ITreeElement
  ////////////////////////////////////

  public Object getParent() {
    return parent;
  }

  public String getText() {
    return location.lastSegment();
  }
}
