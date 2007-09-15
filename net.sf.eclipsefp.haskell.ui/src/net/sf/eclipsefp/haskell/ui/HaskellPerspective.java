// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui;

import net.sf.eclipsefp.common.ui.FPPerspective;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;

import net.sf.eclipsefp.haskell.ui.views.mbview.ModuleBrowser;
import net.sf.eclipsefp.haskell.ui.wizards.NewModuleWizard;


/** <p>The perspective for Haskell development.</p> 
  * 
  * @author Leif Frenzel
  */
public class HaskellPerspective extends FPPerspective {

  @Override
  protected void defineActions( final IPageLayout layout ) {
    // defines common layout from the FP perspectives superclass
    super.defineActions( layout );
    // Add to "Open Perspective" menu    
    layout.addPerspectiveShortcut( HaskellPerspective.class.getName() );
    // add to "Show View" menu
    layout.addShowViewShortcut( ModuleBrowser.ID );
//    layout.addShowViewShortcut( ModuleDependenciesView.ID );
  }
  
  @Override
  protected void addLeftViews( final IFolderLayout left ) {
    super.addLeftViews( left );
    left.addView( ModuleBrowser.ID );
//    left.addView( ModuleDependenciesView.ID );
  }
  
  @Override
  protected void addNewShortcuts( final IPageLayout layout ) {
    layout.addNewWizardShortcut( NewModuleWizard.ID );
    super.addNewShortcuts( layout );
  }
}