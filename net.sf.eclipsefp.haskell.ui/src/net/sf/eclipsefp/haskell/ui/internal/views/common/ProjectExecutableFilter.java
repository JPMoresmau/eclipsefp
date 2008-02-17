// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.common;

import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

/** <p>common filter that filters out project executables in Haskell
  * projects.</p>
  *
  * @author Leif Frenzel
  */
public class ProjectExecutableFilter extends ViewerFilter {


  // interface methods of ViewerFilter
  ////////////////////////////////////

  @Override
  public boolean select( final Viewer viewer,
                         final Object parentElement,
                         final Object element ) {
    return !isProjectExecutable( element );
  }


  // helping functions
  ////////////////////

  private boolean isProjectExecutable( final Object element ) {
    return    element instanceof IFile
           && ResourceUtil.isProjectExecutable( ( IFile )element );
  }
}
