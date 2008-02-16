// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.ISavedState;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;


/** <p>This is Halamos task force responsible for handling the workspace save
  * mechanism. It manages also to register at the workspace for notification
  * at startup.</p>
  *
  * <p>lazy Singleton</p>
  *
  * @author Leif Frenzel
  */
class WSSaveParticipant implements ISaveParticipant {

  /** the singleton instance of WSSaveParticipant. */
//  private static WSSaveParticipant _instance;

  /** private constructor in order to prevent instantiation from outside. */
  private WSSaveParticipant() {
    // we're firing up, so let the resource monitor first be notified about
    // any changes that appeared while we were down
    ISavedState lastState = getLastState();
    if( lastState != null ) {
      IResourceChangeListener monitor = new WorkspaceChangeMonitor();
      lastState.processResourceChangeEvents( monitor );
    }
  }

  /** <p>returns a reference to the singleton instance of
    * WSSaveParticipant.</p> */
  static void initialize() {
//    _instance = new WSSaveParticipant();
  }


  // interface methods of ISaveParticipant
  ////////////////////////////////////////

  public void doneSaving( final ISaveContext context ) {
    // unused
  }

  public void prepareToSave( final ISaveContext context ) {
    // unused
  }

  public void rollback( final ISaveContext context ) {
    // unused
  }

  public void saving( final ISaveContext context ) {
    // request a resource delta to be used on next startup
    context.needDelta();
  }


  // helping methods
  //////////////////

  private ISavedState getLastState() {
    ISavedState result = null;
    try {
      IWorkspace ws = ResourcesPlugin.getWorkspace();
      HaskellCorePlugin plugin = HaskellCorePlugin.getDefault();
      result = ws.addSaveParticipant( plugin, this );
    } catch( CoreException ex ) {
      HaskellCorePlugin.log( "Could not add save participant.", ex ); //$NON-NLS-1$
    }
    return result;
  }
}