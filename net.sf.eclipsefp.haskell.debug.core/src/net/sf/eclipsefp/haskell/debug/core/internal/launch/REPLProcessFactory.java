/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core.internal.launch;

import java.util.Map;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.IProcessFactory;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.eclipse.debug.core.model.RuntimeProcess;


/**
 * @author JP Moresmau
 *
 */
public class REPLProcessFactory implements IProcessFactory {

  /* (non-Javadoc)
   * @see org.eclipse.debug.core.IProcessFactory#newProcess(org.eclipse.debug.core.ILaunch, java.lang.Process, java.lang.String, java.util.Map)
   */
  @Override
  public IProcess newProcess( final ILaunch launch, final Process process,
      final String name, final Map attributes ) {
    return new REPLProcess( launch, process, name, attributes );
  }


  public class REPLProcess extends RuntimeProcess {



    public REPLProcess( final ILaunch launch, final Process process, final String name,
        final Map attributes ) {
      super( launch, process, name, attributes );
    }

    /* (non-Javadoc)
     * @see org.eclipse.debug.core.model.RuntimeProcess#getSystemProcess()
     */
    @Override
    public Process getSystemProcess() {
      return super.getSystemProcess();
    }

    /* (non-Javadoc)
     * @see org.eclipse.debug.core.model.RuntimeProcess#createStreamsProxy()
     */
    @Override
    protected IStreamsProxy createStreamsProxy() {
     return null;
    }
  }
}
