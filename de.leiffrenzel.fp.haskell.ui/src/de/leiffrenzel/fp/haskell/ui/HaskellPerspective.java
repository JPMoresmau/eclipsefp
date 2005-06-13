// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui;

import net.sf.eclipsefp.common.ui.FPPerspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;

import de.leiffrenzel.fp.haskell.ui.views.coview.CompilerOutputView;
import de.leiffrenzel.fp.haskell.ui.views.mbview.ModuleBrowser;
import de.leiffrenzel.fp.haskell.ui.wizards.NewModuleWizard;


/** <p>The perspective for Haskell development.</p> 
  * 
  * @author Leif Frenzel
  */
public class HaskellPerspective extends FPPerspective {

  protected void defineActions( final IPageLayout layout ) {
    // defines common layout from the FP perspectives superclass
    super.defineActions( layout );
    // Add to "Open Perspective" menu    
    layout.addPerspectiveShortcut( HaskellPerspective.class.getName() );
    // add to "Show View" menu
    layout.addShowViewShortcut( CompilerOutputView.ID );
    layout.addShowViewShortcut( ModuleBrowser.ID );
//    layout.addShowViewShortcut( ModuleDependenciesView.ID );
  }
  
  protected void addLeftViews( final IFolderLayout left ) {
    super.addLeftViews( left );
    left.addView( ModuleBrowser.ID );
//    left.addView( ModuleDependenciesView.ID );
  }
  
  protected void addBottomViews( final IFolderLayout bottom ) {
    // add the standard views
    super.addBottomViews( bottom );
    bottom.addView( CompilerOutputView.ID );
  }
  
  protected void addNewShortcuts( final IPageLayout layout ) {
    layout.addNewWizardShortcut( NewModuleWizard.ID );
    super.addNewShortcuts( layout );
  }
}