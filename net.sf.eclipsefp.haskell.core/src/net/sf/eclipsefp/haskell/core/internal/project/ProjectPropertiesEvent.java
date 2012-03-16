// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.project.IProjectPropertiesEvent;


/** <p>implements IProjectPropertiesEvent for use by the Haskell project
  * classes in this package.</p>
  *
  * @author Leif Frenzel
  */
@Deprecated
public class ProjectPropertiesEvent implements IProjectPropertiesEvent {

  private final IHaskellProject source;
  private final String propertyName;
  private Object oldValue;
  private Object newValue;

  public ProjectPropertiesEvent( final IHaskellProject source,
                                 final String propertyName ) {
    this.source = source;
    this.propertyName = propertyName;
  }

  public void setOldValue( final Object oldValue ) {
    this.oldValue = oldValue;
  }

  public void setNewValue( final Object newValue ) {
    this.newValue = newValue;
  }


  // interface methods of IProjectPropertiesEvent
  ///////////////////////////////////////////////

  @Override
  public IHaskellProject getSource() {
    return source;
  }

  @Override
  public String getPropertyName() {
    return propertyName;
  }

  @Override
  public Object getOldValue() {
    return oldValue;
  }

  @Override
  public Object getNewValue() {
    return newValue;
  }
}