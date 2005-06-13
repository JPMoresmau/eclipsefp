// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.project;


/** <p>implements IProjectPropertiesEvent for use by the Haskell project
  * classes in this package.</p>
  * 
  * @author Leif Frenzel
  */
class ProjectPropertiesEvent implements IProjectPropertiesEvent {

  private IHaskellProject source;
  private String propertyName;
  private Object oldValue;
  private Object newValue;

  ProjectPropertiesEvent( final IHaskellProject source,
                          final String propertyName ) {
    this.source = source;
    this.propertyName = propertyName;
  }

  void setOldValue( final Object oldValue ) {
    this.oldValue = oldValue;
  }
  
  void setNewValue( final Object newValue ) {
    this.newValue = newValue;
  }
  
  
  // interface methods of IProjectPropertiesEvent
  ///////////////////////////////////////////////
  
  public IHaskellProject getSource() {
    return source;
  }

  public String getPropertyName() {
    return propertyName;
  }

  public Object getOldValue() {
    return oldValue;
  }

  public Object getNewValue() {
    return newValue;
  }
}