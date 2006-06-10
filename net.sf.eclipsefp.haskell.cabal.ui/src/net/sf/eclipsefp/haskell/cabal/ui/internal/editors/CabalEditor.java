// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors;

import net.sf.eclipsefp.haskell.cabal.core.model.PackageDescription;
import net.sf.eclipsefp.haskell.cabal.ui.internal.editors.outline.CabalOutlinePage;

import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

/** <p>an editor for Cabal package description files.</p> 
  * 
  * <p>Note: this class is declared in <code>plugin.xml</code>.</p>
  *
  * @author Leif Frenzel
  */
public class CabalEditor extends TextEditor {

  private CabalOutlinePage outlinePage;
  private PackageDescription packageDescription;
  
  public CabalEditor() {
    setSourceViewerConfiguration( new CabalConfiguration( this ) );
  }
  
  public void setPackageDescription( final PackageDescription packageDescription ) {
    this.packageDescription = packageDescription;
    if( outlinePage != null ) {
      outlinePage.setPackageDescription( packageDescription );
    }
  }


  
  // interface methods of IAdaptable
  //////////////////////////////////
  
  public Object getAdapter( final Class required ) {
    Object result = null;
    if( IContentOutlinePage.class.equals( required ) ) {
      if( outlinePage == null ) {
        outlinePage = new CabalOutlinePage( packageDescription );
      }
      result = outlinePage;
    }
    return result;
  }
}
