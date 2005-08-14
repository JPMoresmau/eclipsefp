// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui;

import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.ui.*;
import org.eclipse.ui.console.IConsoleConstants;

/** <p>The abstract superclass for all perspectives for fp development.</p> 
  * 
  * <p>Contains configuration for views and editors that are shown when 
  * the user switches to this perspective, and shortcuts to views and 
  * actions that are visible from menus.</p>
  * 
  * @author Leif Frenzel
  */
public class FPPerspective implements IPerspectiveFactory {

  // TODO add JavaDoc comments following guidelines
  
  public void createInitialLayout( final IPageLayout layout ) {
    defineActions( layout );
    defineLayout( layout );
  }

  protected void defineActions( final IPageLayout layout ) {
    // Add to "New" menu
    addNewShortcuts( layout );
    // Add to "Show View" menu
    layout.addShowViewShortcut( IPageLayout.ID_RES_NAV );
    layout.addShowViewShortcut( IPageLayout.ID_BOOKMARKS );
    layout.addShowViewShortcut( IPageLayout.ID_PROBLEM_VIEW );
    layout.addShowViewShortcut( IPageLayout.ID_PROP_SHEET );
    layout.addShowViewShortcut( IPageLayout.ID_TASK_LIST );
    layout.addShowViewShortcut( IPageLayout.ID_OUTLINE );
    layout.addShowViewShortcut( IConsoleConstants.ID_CONSOLE_VIEW );
    // Add toolbar and menu actions
    layout.addActionSet( IDebugUIConstants.LAUNCH_ACTION_SET );
  }
  
  public void defineLayout( final IPageLayout layout ) {
    String editorArea = layout.getEditorArea();

    IFolderLayout left = layout.createFolder( "left", 
                                              IPageLayout.LEFT, 
                                              0.26f, 
                                              editorArea );
    addLeftViews( left );
    IFolderLayout bottom = layout.createFolder( "bottom", 
                                                IPageLayout.BOTTOM, 
                                                0.74f, 
                                                editorArea );
    addBottomViews( bottom );
    IFolderLayout right = layout.createFolder( "right", 
                                               IPageLayout.RIGHT,
                                               0.80f,
                                               editorArea );
    right.addView( IPageLayout.ID_OUTLINE );
  }
  
  protected void addLeftViews( final IFolderLayout left ) {
    left.addView( IPageLayout.ID_RES_NAV );
  }
  
  protected void addBottomViews( final IFolderLayout bottom ) {
    bottom.addView( IPageLayout.ID_TASK_LIST );
    bottom.addView( IPageLayout.ID_PROBLEM_VIEW );
    bottom.addView( IConsoleConstants.ID_CONSOLE_VIEW );
  }
  
  protected void addNewShortcuts( final IPageLayout layout ) {
    layout.addNewWizardShortcut( "org.eclipse.ui.wizards.new.folder" );
    layout.addNewWizardShortcut( "org.eclipse.ui.wizards.new.file" );
  }
}