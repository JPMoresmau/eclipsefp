/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import java.util.ArrayList;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.ui.model.WorkbenchContentProvider;


public class LimitedWorkbenchContentProvider extends WorkbenchContentProvider {

  private final boolean onlyDirs;
  private final boolean showHidden;

  public LimitedWorkbenchContentProvider() {
    this(false, false);
  }

  public LimitedWorkbenchContentProvider(final boolean onlyDirs) {
    this(onlyDirs, false);
  }

  public LimitedWorkbenchContentProvider(final boolean onlyDirs, final boolean showHidden) {
    this.onlyDirs = onlyDirs;
    this.showHidden = showHidden;
  }

  @Override
  public Object[] getElements( final Object element ) {
    return super.getElements( element );
  }

  @Override
  public Object[] getChildren( final Object element ) {
    ArrayList<Object> objs = new ArrayList<>();
    for ( Object inner : super.getChildren( element )) {
      boolean include = true;
      if( !showHidden && ( ( IResource )inner ).getName().startsWith( "." ) ) {
        include = false;
      }
      if (onlyDirs && !(inner instanceof IFolder)) {
        include = false;
      }
      if (include) {
        objs.add( inner );
      }
    }
    return objs.toArray();
  }

//  private Object[] filter(final Object[] elements) {
//    ArrayList<Object> objs = new ArrayList<Object>();
//    for ( Object inner : elements) {
//      IResource resource = (IResource)inner;
//      if (!resource.getName().startsWith( "." )) {
//        objs.add( resource );
//      }
//    }
//    return objs.toArray();
//  }
}
