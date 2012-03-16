// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model;

import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;


public class GHCPackage extends GHCPackageResource {

  private final String nameVersion;
  private final boolean hidden;

  public GHCPackage( final GHCPackageConf packageConf,
                     final String nameVersion,
                     final boolean hidden ) {
    super( packageConf, packageConf.getLocation().removeLastSegments( 1 ) );
    this.nameVersion = nameVersion;
    this.hidden = hidden;
  }


  // interface methods of ITreeElement
  ////////////////////////////////////

  @Override
  public List<GHCPackageResource> getChildren() {
    return Collections.emptyList();
  }

  @Override
  public String getText() {
    // can't use superclass functionality here!
    return nameVersion;
  }

  @Override
  public String getImageKey() {
    return hidden ? IImageNames.HIDDEN_PACKAGE : IImageNames.PACKAGE;
  }
}