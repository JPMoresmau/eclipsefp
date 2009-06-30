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
package net.sf.eclipsefp.haskell.core.util;

import java.io.File;
import java.io.IOException;
import java.io.Writer;

public interface IProcessRunner {

  /**
   * Runs the program, and returns when it completes.
   *
   * @param workingDir the working directory from which the program should be started
   * @param out the {@link Writer} to receive the process's merged stdout/stderr streams
   * @param args the command line
   *
   * @returns all output generated on the program's stdout/stderr streams
   */
  String executeBlocking(File workingDir, Writer out, String... args);

  /**
   * Runs the program and returns immediately.
   *
   * @param workingDir the working directory from which the program should be started
   * @param outs the {@link Writer}s to receive the process's merged stdout/stderr streams
   * @param args the command line
   *
   * @return the running process
   */
  Process executeNonblocking(File workingDir, Writer out, String... args) throws IOException;

}
