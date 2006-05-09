// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;


/** <p>Implementors can register with the <code>HaskellProjectManager</code>
  * to be notified about changes in <code>IHaskellProject</code>s.</p>
  *
  * @see HaskellProjectManager.addProjectPropertiesListener()
  * @see IProjectPropertiesEvent
  *  
  * @author Leif Frenzel
  */
public interface IProjectPropertiesListener {
 
  /** <p>called when properties of IHaskellProjects have changed. The passed
    * event object contains information about the changed property and
    * the project where the change occured.</p> */
  void projectPropertyChanged( IProjectPropertiesEvent event );
}