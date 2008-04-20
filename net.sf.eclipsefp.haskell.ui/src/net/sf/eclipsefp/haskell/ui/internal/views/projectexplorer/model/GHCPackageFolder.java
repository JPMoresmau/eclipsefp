// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model;

import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.runtime.IPath;


public class GHCPackageFolder extends GHCPackageResource {

  public GHCPackageFolder( final GHCPackageResource parent, final IPath path ) {
    super( parent, path );
  }


  // interface methods of ITreeElement
  ////////////////////////////////////

  public List<?> getChildren() {
    return Collections.emptyList();
  }

  public String getImageKey() {
    return IImageNames.PACKAGE_FOLDER;
  }
}
