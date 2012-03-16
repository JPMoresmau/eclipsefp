// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.preferences.hsimpls;

import java.util.List;

import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

class HsImplementationsCP implements IStructuredContentProvider {

  private final List<IHsImplementation> installations;


  HsImplementationsCP( final List<IHsImplementation> installations ) {
    this.installations = installations;
  }


  // interface methods of IStructuredContentProvider
  //////////////////////////////////////////////////

  @Override
  public Object[] getElements( final Object input ) {
    return installations.toArray();
  }

  @Override
  public void inputChanged( final Viewer viewer,
                            final Object oldInput,
                            final Object newInput ) {
    // unused
  }

  @Override
  public void dispose() {
    // unused
  }
}