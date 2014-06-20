/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.test;

import java.io.Serializable;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.buildwrapper.types.Location;
import org.eclipse.core.resources.IProject;


/**
 * A test result, possibly with sub tests
 * @author JP Moresmau
 *
 */
public class TestResult implements Serializable {
  /**
   *
   */
  private static final long serialVersionUID = 3219320400432673266L;

  /**
   * the status
   */
  private TestStatus status=TestStatus.PENDING;
  /**
   * the test name
   */
  private final String name;
  /**
   * the relevant source code location, if any
   */
  private Location location;
  /**
   * the detail text, if any
   */
  private String text;
  /**
   * the related project, if any
   */
  private IProject project;

  /**
   * wall time in milliseconds
   */
  private long wallTime=-1;

  /**
   * the children
   */
  private final Map<String,TestResult> children=new LinkedHashMap<>();

  public TestResult( final String name ) {
    super();
    this.name = name;
  }


  /**
   * get the status
   * if there are sub tests, the status depends on the status of the children
   * @return
   */
  public TestStatus getStatus() {
    TestStatus stat=children.size()>0?TestStatus.OK:status;
    for (TestResult child:children.values()){
      if (child.getStatus()!=null && stat==null || child.getStatus().ordinal()>stat.ordinal()){
        stat=child.getStatus();
      }
    }
    return stat;
  }

  /**
   * is the test finished?
   * @return
   */
  public boolean isFinished(){
    TestStatus st=getStatus();
    return st!=null && (!st.equals( TestStatus.PENDING ) && !st.equals( TestStatus.RUNNING ));
  }

  public void setStatus( final TestStatus status ) {
    this.status = status;
  }

  public String getName() {
    return name;
  }

  public String getText() {
    return text;
  }

  public void setText( final String text ) {
    this.text = text;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
   return getName();
  }


  public Location getLocation() {
    return location;
  }


  public void setLocation( final Location location ) {
    this.location = location;
  }


  public IProject getProject() {
    return project;
  }


  public void setProject( final IProject project ) {
    this.project = project;
  }


  /**
   * @return the children
   */
  public Collection<TestResult> getChildren() {
    return children.values();
  }

  public void addChild(final TestResult tr){
    children.put( tr.getName(), tr );
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( name == null ) ? 0 : name.hashCode() );
    return result;
  }


  @Override
  public boolean equals( final Object obj ) {
    if( this == obj ) {
      return true;
    }
    if( obj == null ) {
      return false;
    }
    if( getClass() != obj.getClass() ) {
      return false;
    }
    TestResult other = ( TestResult )obj;
    if( name == null ) {
      if( other.name != null ) {
        return false;
      }
    } else if( !name.equals( other.name ) ) {
      return false;
    }
    return true;
  }



  public long getWallTime() {
    if (wallTime==-1){
      long wt=0;
      for (TestResult child:children.values()){
        wt+=child.getWallTime();
      }
      return wt;
    }
    return wallTime;
  }



  public void setWallTime( final long wallTime ) {
    this.wallTime = wallTime;
  }
}
