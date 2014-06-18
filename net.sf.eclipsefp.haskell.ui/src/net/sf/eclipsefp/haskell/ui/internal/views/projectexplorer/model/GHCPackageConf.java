// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.types.CabalPackage;
import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.runtime.IPath;

/** <p>a package.conf descriptor (used by GHC to remember where it keeps
  * registered packages).</p>
  *
  * @author Leif Frenzel
  */
public class GHCPackageConf implements ITreeElement {

  private final IPath location;
  private final GHCSystemLibrary systemLibrary;
//  private final String content;

  private List<GHCPackage> children=null;
//
//  public GHCPackageConf( final GHCSystemLibrary systemLibrary,
//                         final IPath location,
//                         final String content ) {
//    this.systemLibrary = systemLibrary;
//    this.location = location;
//    this.content = content;
//  }

  public GHCPackageConf( final GHCSystemLibrary systemLibrary,
      final IPath location,
      final CabalPackage[] pkgs ) {
    this.systemLibrary = systemLibrary;
    this.location = location;
//    this.content=null;
    children=new ArrayList<>();
    for (CabalPackage pkg:pkgs){
      // only referenced libraries!
      if (pkg.getComponents().length>0){
        children.add(new GHCPackage( this, pkg.getName()+"-"+pkg.getVersion(), !pkg.isExposed() ));
      }
    }
  }

  IPath getLocation() {
    return location;
  }


  // interface methods of ITreeElement
  ////////////////////////////////////

  @Override
  public List<GHCPackage> getChildren() {
//    if (children==null){
//      List<GHCPackage> result = new ArrayList<GHCPackage>();
//      parseContent( result );
//      children=result;
//    }
    return children;
  }

  @Override
  public Object getParent() {
    return systemLibrary;
  }

  @Override
  public String getText() {
    return location.toOSString();
  }

  @Override
  public String getImageKey() {
    return IImageNames.PACKAGE_CONF;
  }


  // helping functions
  ////////////////////

//  private void parseContent( final List<GHCPackage> pkgs ) {
//    StringTokenizer tok = new StringTokenizer( content, ",", false ); //$NON-NLS-1$
//    while( tok.hasMoreTokens() ) {
//      String token = tok.nextToken().trim();
//      boolean hidden = false;
//      if( token.startsWith( "(" ) && token.endsWith( ")" ) ) { //$NON-NLS-1$ //$NON-NLS-2$
//        hidden = true;
//        token = token.substring( 1, token.length() - 1 );
//      }
//      pkgs.add( new GHCPackage( this, token, hidden ) );
//    }
//  }
}
