/*******************************************************************************
 * Copyright (c) 2005, 2006 Thiago Arrais and others.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Thiago Arrais - Initial API and implementation
 *******************************************************************************/
package net.sf.eclipsefp.haskell.util;

import java.io.File;
import java.io.IOException;
import java.io.Writer;

public interface IProcessRunner {

  /**
   * Runs the program, and returns when it completes.
   *
   * @param workingDir the working directory from which the program should be started
   * @param out the {@link Writer} to receive the process's stdout stream (may be equal to err, but not null)
   * @param err the {@link Writer} to receive the process's stderr stream (may be equal to out, but not null)
   * @param args the command line
   *
   * @see NullWriter
   */
  int executeBlocking(File workingDir, Writer out, Writer err, String... args) throws IOException;

  /**
   * Runs the program and returns immediately.
   *
   * @param workingDir the working directory from which the program should be started
   * @param out the {@link Writer} to receive the process's stdout stream (may be equal to err, but not null)
   * @param err the {@link Writer} to receive the process's stderr stream (may be equal to out, but not null)
   * @param args the command line
   *
   * @return the running process
   *
   * @see NullWriter
   */
  Process executeNonblocking(File workingDir, Writer out, Writer err, String... args) throws IOException;

}
