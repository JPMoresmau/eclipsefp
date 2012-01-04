/** 
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.buildwrapper.types;



/**
 * Interface for a handler of an outline call
 * @author JP Moresmau
 *
 */
public interface OutlineHandler  {

  /**
   * handle the result of the outline call
   * @param result the outline result
   */
  void handleOutline(OutlineResult result);
}
