/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.test;



/**
 * Test listener for UI viewing, etc...
 * @author JP Moresmau
 *
 */
public interface ITestListener {
  /**
   * start a test suite
   * @param suite
   */
  public void start(TestSuite suite);

  /**
   * test suite updated
   * @param suite
   */
  public void update(TestSuite suite);

  /**
   * test suite complete
   * @param suite
   */
  public void end(TestSuite suite);
}
