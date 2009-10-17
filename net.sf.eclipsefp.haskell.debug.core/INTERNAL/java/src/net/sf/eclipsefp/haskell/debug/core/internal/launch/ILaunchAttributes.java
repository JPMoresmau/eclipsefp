// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.core.internal.launch;

/** <p>contains constant name defintions for launch configuration
  * attributes.</p>
  *
  * @author Leif Frenzel
  */
public interface ILaunchAttributes {

  String EMPTY = ""; //$NON-NLS-1$

  String RUN_IN_BACKGROUND = "RUN_IN_BACKGROUND"; //$NON-NLS-1$
  String WORKING_DIRECTORY = "WORKING_DIRECTORY";   //$NON-NLS-1$
  String ARGUMENTS         = "ARGUMENTS"; //$NON-NLS-1$
  String EXECUTABLE        = "EXECUTABLE"; //$NON-NLS-1$
  String PROJECT_NAME      = "PROJECT_NAME"; //$NON-NLS-1$

  String SYNC_STREAMS     = "SYNC_STREAMS"; //$NON-NLS-1$
  String RELOAD_COMMAND   = "RELOAD_COMMAND";//$NON-NLS-1$
  String RELOAD           = "RELOAD";//$NON-NLS-1$
  String COMMAND          = "COMMAND";//$NON-NLS-1$
  String COMMAND_ON_RELOAD= "COMMAND_ON_RELOAD";//$NON-NLS-1$
}