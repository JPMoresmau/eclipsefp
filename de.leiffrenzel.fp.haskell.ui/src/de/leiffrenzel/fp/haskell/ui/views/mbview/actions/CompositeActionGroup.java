// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.views.mbview.actions;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.util.Assert;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.actions.ActionGroup;


/** <p>Action group composite, because we have several action groups 
  * within the Module Browser's action group.</p>
  * 
  * @author Leif Frenzel
  */
class CompositeActionGroup extends ActionGroup {

  private ActionGroup[] fGroups;
  
  protected void setGroups( final ActionGroup[] groups ) {
    Assert.isTrue( fGroups == null );
    Assert.isNotNull( groups );
    fGroups = groups;
  }

  public ActionGroup get( final int index ) {
    ActionGroup result = null;
    if( fGroups != null ) {
      result = fGroups[ index ];
    }
    return result;
  }
  
  public void addGroup( final ActionGroup group ) {
    if( fGroups == null ) {
      fGroups = new ActionGroup[] { group };
    } else {
      ActionGroup[] newGroups = new ActionGroup[ fGroups.length + 1 ];
      System.arraycopy( fGroups, 0, newGroups, 0, fGroups.length );
      newGroups[ fGroups.length ] = group;
      fGroups = newGroups;
    }
  }
  
  public void dispose() {
    super.dispose();
    if( fGroups != null ) {
      for( int i = 0; i < fGroups.length; i++ ) {
        fGroups[ i ].dispose();
      }
    }
  }

  public void fillActionBars( final IActionBars actionBars ) {
    super.fillActionBars( actionBars );
    if( fGroups != null ) {
      for( int i = 0; i < fGroups.length; i++ ) {
        fGroups[ i ].fillActionBars( actionBars );
      }
    }
  }

  public void fillContextMenu( final IMenuManager menu ) {
    super.fillContextMenu( menu );
    if( fGroups != null ) {
      for( int i = 0; i < fGroups.length; i++ ) {
        fGroups[ i ].fillContextMenu( menu );
      }
    }
  }

  public void setContext( final ActionContext context ) {
    super.setContext( context );
    if( fGroups != null ) {
      for( int i = 0; i < fGroups.length; i++ ) {
        fGroups[ i ].setContext( context );
      }
    }
  }

  public void updateActionBars() {
    super.updateActionBars();
    if( fGroups != null ) {
      for( int i = 0; i < fGroups.length; i++ ) {
        fGroups[ i ].updateActionBars();
      }
    }
  }
}