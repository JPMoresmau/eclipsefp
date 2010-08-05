// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.halamo;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.ISavedState;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Plugin;


/** <p>This is Halamos task force responsible for handling the workspace save
  * mechanism. It manages also to register at the workspace for notification
  * at startup.</p>
  *
  * <p>lazy Singleton</p>
  *
  * @author Leif Frenzel
  */
class WSSaveParticipant implements ISaveParticipant {

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
    new WSSaveParticipant();
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
         /* TODO: Collapse this code to when Galileo is no longer supported:
          *
          * try {
          *   ws.addSaveParticipant(HaskellCorePlugin.getPluginId(), this);
          * } catch (CoreException e) {
          *   HaskellCorePlugin.log("CoreException in addSaveParticipant", e);
          * }
          */
          ISavedState result = null;
          try {
            HaskellCorePlugin plugin = HaskellCorePlugin.getDefault();
            IWorkspace ws = ResourcesPlugin.getWorkspace();
           Method addSaveParticipantM = addSaveParticipant_Helios(ws);

           Object[] args = null;
           if (addSaveParticipantM != null) {
             // Helios API
             args = new Object[] { HaskellCorePlugin.getPluginId(), this  };
           } else {
             // Galileo API
             addSaveParticipantM = addSaveParticipant_Galileo(ws);
             assert addSaveParticipantM != null;
             args = new Object[] { plugin, this };
           }
           result = (ISavedState) addSaveParticipantM.invoke( ws, args );
         } catch( IllegalArgumentException e ) {
           HaskellCorePlugin.log("Illegal argument exception, addSaveParticipant", e); //$NON-NLS-1$
         } catch( IllegalAccessException e ) {
           HaskellCorePlugin.log("Illegal access exception, addSaveParticipant", e); //$NON-NLS-1$
         } catch( InvocationTargetException e ) {
           Throwable t = e.getCause();
           if (t instanceof CoreException) {
             CoreException coreExc = (CoreException) t;
             HaskellCorePlugin.log("addSaveParticipant exception", coreExc); //$NON-NLS-1$
           } else {
             HaskellCorePlugin.log( "Uncaught/unknown invocation exception", t ); //$NON-NLS-1$
           }
         }
         return result;
       }

       // Helios API version of addSaveParticipant
       private Method addSaveParticipant_Helios(final IWorkspace ws)
       {
         Method result = null;
         Class<? extends Object>[] parmsHelios = new Class<?>[] {
             String.class,
             ISaveParticipant.class
         };

         try {
           result = ws.getClass().getMethod("addSaveParticipant", parmsHelios); //$NON-NLS-1$
         }
         catch (NoSuchMethodException excM) {
           // Ignore
         }
         return result;
       }

       private Method addSaveParticipant_Galileo(final IWorkspace ws)
       {
         Method result = null;
         Class<? extends Object>[] parmsGalileo = new Class<?>[] {
             Plugin.class,
             ISaveParticipant.class
           };
         try {
           result = ws.getClass().getMethod("addSaveParticipant", parmsGalileo); //$NON-NLS-1$
         }
         catch (NoSuchMethodException excM) {
           // Ignore
          }

          return result;
        }

}