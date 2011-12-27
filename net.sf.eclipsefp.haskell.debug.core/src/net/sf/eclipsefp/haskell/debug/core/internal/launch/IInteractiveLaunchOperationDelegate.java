// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;

/** <p>Implementors tell the <code>AbstractInteractiveLaunchShortcut</code>
  * the essential things for launching an interactive environment for a
  * source file (or set of source files).</p>
  *
  * <p>The combination of a subclass of
  * <code>AbstractInteractiveLaunchShortcut</code> and an implementation of
  * this interface can be used by plugins that run interactive environmens.</p>
  *
  * @author Leif Frenzel
  */
public interface IInteractiveLaunchOperationDelegate {

  /** <p>returns the name of the executable. This is used to run
    * <code>Runtime.exec()</code> and should therefore name an existing and
    * executable file on the system.</p> */
  String getExecutable();

  /** <p>creates the command line arguments (list the modules to be loaded,
    * library paths etc.).</p> */
  String[] createArguments( IProject hsProject, IFile[] files );

  /**
   * get the reload command or null if not supported
   * @return the reload command, or null
   */
  String getReloadCommand();
}
