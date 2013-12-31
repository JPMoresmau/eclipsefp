/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.CabalBenchLaunchDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ILaunchAttributes;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;


/**
 * Operation for "cabal bench"
 * @author JP Moresmau
 *
 */
public class CabalBenchLaunchOperation extends CabalTestLaunchOperation {
  public static final String CABALBENCHCONFIG_TYPE = CabalBenchLaunchDelegate.class.getName();

  /**
   *
   */
  public CabalBenchLaunchOperation() {
  }


  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.ui.internal.launch.LaunchOperation#getConfigTypeName()
   */
  @Override
  protected String getConfigTypeName() {
    return CABALBENCHCONFIG_TYPE;
  }

  @Override
  protected ILaunchConfiguration createConfiguration(final IProject proj )
      throws CoreException {
    ILaunchConfigurationType configType = getConfigType();
    String id = createConfigId( proj.getName()+" bench" ); //$NON-NLS-1$
    ILaunchConfigurationWorkingCopy wc = configType.newInstance( null, id );
    wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, proj.getLocation().toOSString() );
        //proj.getLocation().append( BWFacade.DIST_FOLDER).toOSString() );
    wc.setAttribute( ILaunchAttributes.PROJECT_NAME, proj.getName() );
    wc.setAttribute( ILaunchAttributes.SYNC_STREAMS, true );
    String s=proj.getLocation().append( BWFacade.DIST_FOLDER_CABAL).toOSString();

    wc.setAttribute( ILaunchAttributes.EXTRA_ARGUMENTS,"bench \"--builddir="+s+"\""); //$NON-NLS-1$ //$NON-NLS-2$
    return wc.doSave();
  }
}
