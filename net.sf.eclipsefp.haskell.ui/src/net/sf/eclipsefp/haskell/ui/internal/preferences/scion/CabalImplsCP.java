// Copyright (c) 2010 B. Scott Michel (scottm@aero.org)
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import java.util.List;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementation;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

/** Structured content provider for the Cabal implementations preference page.
 *
 * @author B. Scott Michel (scottm@aero.org)
 */
public class CabalImplsCP implements IStructuredContentProvider {
  private final List<CabalImplementation> impls;


  CabalImplsCP( final List<CabalImplementation> impls ) {
    this.impls = impls;
  }

  public void dispose() {
    // Unused.
  }

  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput ) {
    // unused

  }

  public Object[] getElements( final Object inputElement ) {
    return impls.toArray();
  }

}
