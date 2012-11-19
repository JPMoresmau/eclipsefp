/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.test;

import java.io.Serializable;
import java.util.Collection;
import java.util.concurrent.atomic.AtomicInteger;


/**
 * Top level structure for tests
 * @author JP Moresmau
 *
 */
public class TestSuite implements Serializable {
  /**
   *
   */
  private static final long serialVersionUID = -611295970696203914L;
  static final AtomicInteger allRunIDs=new AtomicInteger(0);
  /**
   * unique ID
   */
  private final int id=allRunIDs.getAndIncrement();

  /**
   * the root test result
   */
  private final TestResult root;
  private int runs=0;
  private int errors=0;
  private int failures=0;

  public TestSuite(final TestResult root){
    this.root=root;
    count(root);
  }

  /**
   * calculate stats
   * @param tr
   */
  private void count(final TestResult tr){
    Collection<TestResult> cs=tr.getChildren();
    if (cs.isEmpty()){
      switch (tr.getStatus()){
        case ERROR:
            errors++;
            runs++;
            break;
        case FAILURE:
            failures++;
            runs++;
            break;
        case OK:
            runs++;
            break;
        case PENDING:
        case RUNNING:
          break;
      }

    } else {
      for (TestResult c:cs){
        count(c);
      }
    }
  }


  /**
   * @return the runID
   */
  public int getID() {
    return id;
  }

  /**
   * @return the root
   */
  public TestResult getRoot() {
    return root;
  }

  public String getName(){
    return getRoot().getName();
  }


  public int getRuns() {
    return runs;
  }


  public int getErrors() {
    return errors;
  }


  public int getFailures() {
    return failures;
  }

  /* (non-Javadoc)
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return getName();
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + id;
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
    TestSuite other = ( TestSuite )obj;
    if( id != other.id ) {
      return false;
    }
    return true;
  }
}
