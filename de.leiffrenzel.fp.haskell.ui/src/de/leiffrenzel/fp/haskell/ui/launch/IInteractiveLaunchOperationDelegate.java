//Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.launch;

import org.eclipse.core.resources.IFile;

import de.leiffrenzel.fp.haskell.core.project.IHaskellProject;

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
  String[] createArguments( IHaskellProject hsProject, IFile[] files );
}
