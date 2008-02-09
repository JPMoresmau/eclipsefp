// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.configurator;

import org.eclipse.core.runtime.IProgressMonitor;

/** <p>Implementors perform a long-running operation (e.g. find an executable
  * on the system).</p>
  *  
  * @author Leif Frenzel
  */
public interface IProbe {

  /** <p>implemented to execute the probing. The return value of this method
    * will be passed to the method {link IConfiguratorPage#probed(Object)}
    * of the {@link IConfiguratorPage IConfiguratorPage} which declared this
    * <code>IProbe</code>.<p/>*/
  Object execute( final IProgressMonitor monitor );
}
