// Copyright (c) 2003-2007 by Leif Frenzel - see http://leiffrenzel.de
// Copyright (c) 2011 by Alejandro Serrano
package net.sf.eclipsefp.haskell.ui;

import net.sf.eclipsefp.haskell.browser.BrowserPerspective;
import net.sf.eclipsefp.haskell.browser.views.hoogle.HoogleView;
import net.sf.eclipsefp.haskell.ui.views.CabalPackagesView;
import net.sf.eclipsefp.haskell.ui.wizards.NewModuleWizard;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.progress.IProgressConstants;


/** <p>The perspective for Haskell development.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellPerspective implements IPerspectiveFactory {


  // interface methods of IPerspectiveFactory
  ///////////////////////////////////////////

  @Override
  public void createInitialLayout( final IPageLayout layout ) {
    defineActions( layout );
    defineLayout( layout );
  }


  // helping functions
  ////////////////////

  private void defineActions( final IPageLayout layout ) {
    // Add to "New" menu
    addNewShortcuts( layout );
    // Add to "Show View" menu
    layout.addShowViewShortcut( "org.eclipse.ui.navigator.ProjectExplorer" ); //$NON-NLS-1$
    layout.addShowViewShortcut( IPageLayout.ID_BOOKMARKS );
    layout.addShowViewShortcut( IPageLayout.ID_PROBLEM_VIEW );
    layout.addShowViewShortcut( IPageLayout.ID_PROP_SHEET );
    layout.addShowViewShortcut( IPageLayout.ID_TASK_LIST );
    layout.addShowViewShortcut( IPageLayout.ID_OUTLINE );
    layout.addShowViewShortcut( IConsoleConstants.ID_CONSOLE_VIEW );
    layout.addShowViewShortcut( CabalPackagesView.ID );
    layout.addShowViewShortcut( "net.sf.eclipsefp.haskell.debug.ui.test.TestResultView" );
    // Add toolbar and menu actions
    layout.addActionSet( IDebugUIConstants.LAUNCH_ACTION_SET );
    // Add to "Open Perspective" menu
    layout.addPerspectiveShortcut( HaskellPerspective.class.getName() );
    layout.addPerspectiveShortcut( BrowserPerspective.class.getName() );
    // Add "Haskell Project" to the File -> New menu
    layout.addNewWizardShortcut("net.sf.eclipsefp.haskell.ui.wizards.NewHaskellProjectWizard"); //$NON-NLS-1$
  }

  private void defineLayout( final IPageLayout layout ) {
    String editorArea = layout.getEditorArea();

    IFolderLayout left = layout.createFolder( "left", //$NON-NLS-1$
                                              IPageLayout.LEFT,
                                              0.26f,
                                              editorArea );
    addLeftViews( left );
    IFolderLayout bottom = layout.createFolder( "bottom", //$NON-NLS-1$
                                                IPageLayout.BOTTOM,
                                                0.74f,
                                                editorArea );
    addBottomViews( bottom );
    IFolderLayout right = layout.createFolder( "right", //$NON-NLS-1$
                                               IPageLayout.RIGHT,
                                               0.75f,
                                               editorArea );
    right.addView( IPageLayout.ID_OUTLINE );
    right.addView( HoogleView.ID );
    right.addView( CabalPackagesView.ID);
  }

  private void addLeftViews( final IFolderLayout left ) {
    left.addView( "org.eclipse.ui.navigator.ProjectExplorer" ); //$NON-NLS-1$
    left.addView( "net.sf.eclipsefp.haskell.debug.ui.test.TestResultView" );
  }

  private void addBottomViews( final IFolderLayout bottom ) {
    bottom.addView( IPageLayout.ID_TASK_LIST );
    bottom.addView( IPageLayout.ID_PROBLEM_VIEW );
    bottom.addView( IConsoleConstants.ID_CONSOLE_VIEW );
    bottom.addView( IProgressConstants.PROGRESS_VIEW_ID);

  }

  private void addNewShortcuts( final IPageLayout layout ) {
    layout.addNewWizardShortcut( "org.eclipse.ui.wizards.new.folder" ); //$NON-NLS-1$
    layout.addNewWizardShortcut( "org.eclipse.ui.wizards.new.file" ); //$NON-NLS-1$
    layout.addNewWizardShortcut( NewModuleWizard.ID );
  }
}