// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.views.mbview.actions;

import org.eclipse.jface.action.*;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.ui.actions.ActionGroup;


/** <p>TODO</p>
  * 
  * @author Leif Frenzel
  * 
  * taken from <code>org.eclipse.jdt.internal.ui.actions</code>
  * 
  * A MultiActionGroup will display a list of IActions in a menu by 
  * transforming them into MenuItems. The list of labels given will be 
  * what is displayed in the ViewMenu for the corresponding action (the 
  * action at the same position in the action array). The actions are 
  * currently implemented as state based so that after an action is 
  * executed the label will have a selection check.
  */
public class MultiActionGroup extends ActionGroup {
  
  private IAction[] fActions; 
  
  private int currentSelection;
  private MenuItem[] items;

  void init( final IAction[] actions, final int currentSelection ) {
    this.currentSelection = currentSelection;
    this.fActions = actions;
    items = new MenuItem[ fActions.length ];    
  }
  
  protected void addActions( final IMenuManager viewMenu ) {
    viewMenu.add( new Separator() );
    for( int i = 0; i < fActions.length; i++ ) {
      ActionContributionItem item 
        = new ActionContributionItem( this, 
                                      fActions[ i ].getText(),
                                      fActions[ i ].getImageDescriptor(),
                                      i );
      viewMenu.add( item );
    }
  }

  private void setMenuItem( final MenuItem item, final int index ) {
    items[ index ] = item;
  }
  
  // TODO bad design, refactor
  
  // inner classes
  ////////////////

  private class ActionContributionItem extends ContributionItem {

    private final int j;
    private final MultiActionGroup group;
    private final String text;
    private final ImageDescriptor imgDesc;

    private ActionContributionItem( final MultiActionGroup group,
                                    final String text,
                                    final ImageDescriptor imgDesc,
                                    final int j ) {
      super();
      this.group = group;
      this.text = text;
      this.imgDesc = imgDesc;
      this.j = j;
    }

    @Override
    public void fill( final Menu menu, final int index ) {
      MenuItem menuItem = new MenuItem( menu, SWT.CHECK, index );
      menuItem.setImage( imgDesc.createImage() );
      menuItem.setText( text );
      
      group.setMenuItem( menuItem, j );
      menuItem.setSelection( currentSelection == j );
      menuItem.addSelectionListener( new SelectionAdapter() {
        @Override
        public void widgetSelected( final SelectionEvent e ) {
          if( currentSelection == j ) {
            items[ currentSelection ].setSelection( true );
            return;
          }
          fActions[ j ].run();
          // Update checked state
          items[ currentSelection ].setSelection( false );
          currentSelection = j;
          items[ currentSelection ].setSelection( true );
        }
      } );
    }
  }
}