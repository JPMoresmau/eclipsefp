/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.views;

import net.sf.eclipsefp.haskell.core.cabal.CabalPackageRef;
import net.sf.eclipsefp.haskell.core.cabal.CabalPackageVersion;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.model.WorkbenchViewerComparator;

/**
 * Comparator for cabal packages view objects
 *
 * @author JP Moresmau
 *
 */
public class CabalPackageViewerComparator extends WorkbenchViewerComparator {

  @Override
  public int compare( final Viewer viewer, final Object e1, final Object e2 ) {
    if (e1 instanceof CabalPackageRef && e2 instanceof CabalPackageRef){
      return ((CabalPackageRef)e1).getName().compareToIgnoreCase( ((CabalPackageRef)e2).getName() );
    }
    if (e1 instanceof CabalPackageVersion && e2 instanceof CabalPackageVersion){
      return ((CabalPackageVersion)e1).compareTo( (CabalPackageVersion)e2);
    }
    return super.compare( viewer, e1, e2 );
  }
}
