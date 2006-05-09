// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;


/** <p>Fired by the <code>HaskellProjectManager</code> and received by
  * implementors of <code>IProjectPropertiesListener</code>s when a property
  * of an <code>IHaskellProject</code> has changed.</p>
  * 
  * @author Leif Frenzel
  */
public interface IProjectPropertiesEvent {

  /** <p>returns the Haskell project where the change has occured.</p> */
  IHaskellProject getSource();
  /** <p>returns the name of the property that has changed.</p>
    *
    * <p>The property name is one of the constants beginning with
    * PROPERTY_ defined in IHaskellProject.</p> 
    */
  String getPropertyName();
  /** <p>returns the value the changed property had before the change.</p> */
  Object getOldValue();
  /** <p>returns the new value of the property after the change.</p> */
  Object getNewValue();
}