// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors.outline;

import net.sf.eclipsefp.haskell.cabal.core.model.PackageDescription;

import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

/** <p>content outline for the Cabal editor.</p> 
  * 
  * @author Leif Frenzel 
  */
public class CabalOutlinePage extends ContentOutlinePage {
  
  private PackageDescription packageDescription;

  public CabalOutlinePage( final PackageDescription packageDescription ) {
    this.packageDescription = packageDescription;
  }
  
  public void setPackageDescription( final PackageDescription pd ) {
    this.packageDescription = pd;
    if( getTreeViewer() != null ) {
      getTreeViewer().setInput( pd );
    }
  }
  
  
  // interface methods of ContentOutlinePage
  //////////////////////////////////////////
  
  @Override
  public void createControl( final Composite parent ) {
    super.createControl( parent );
    TreeViewer tv = getTreeViewer();
    tv.setLabelProvider( new CabalOutlineLP() );
    tv.setContentProvider( new CabalOutlinePageCP() );
    tv.setAutoExpandLevel( AbstractTreeViewer.ALL_LEVELS );
    tv.setInput( packageDescription );
  }
}