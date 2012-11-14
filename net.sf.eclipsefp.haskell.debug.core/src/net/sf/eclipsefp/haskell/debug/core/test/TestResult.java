/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.test;

import java.io.Serializable;
import java.util.LinkedList;
import java.util.List;
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
   * the children
   */
  private final List<TestResult> children=new LinkedList<TestResult>();

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
    for (TestResult child:children){
      if (child.getStatus()!=null && stat==null || child.getStatus().ordinal()>stat.ordinal()){
        stat=child.getStatus();
      }
    }
    return stat;
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
  public List<TestResult> getChildren() {
    return children;
  }
}
