/**
 *  Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import net.sf.eclipsefp.haskell.buildwrapper.BWFacade;
import net.sf.eclipsefp.haskell.compat.ILaunchManagerCompat;
import net.sf.eclipsefp.haskell.core.util.ResourceUtil;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.CabalTestLaunchDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ILaunchAttributes;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;


/**
 * Operation for "cabal test"
 * @author JP Moresmau
 *
 */
public class CabalTestLaunchOperation extends LaunchOperation {
  public static final String CABALTEST_CONFIG_TYPE = CabalTestLaunchDelegate.class.getName();


  /**
   *
   */
  public CabalTestLaunchOperation() {
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.debug.ui.internal.launch.LaunchOperation#getConfigTypeName()
   */
  @Override
  protected String getConfigTypeName() {
    return CABALTEST_CONFIG_TYPE;
  }

  public List<ILaunchConfiguration> findConfiguration(
      final IProject project) throws CoreException {
    List<ILaunchConfiguration> result = new LinkedList<ILaunchConfiguration>();
    ILaunchConfiguration[] configurations = LaunchOperation
        .getConfigurations( LaunchOperation
            .getConfigType( getConfigTypeName() ) );

    result = new ArrayList<ILaunchConfiguration>( configurations.length );
    for( int i = 0; i < configurations.length; i++ ) {
      ILaunchConfiguration configuration = configurations[ i ];
      String thisProject=configuration.getAttribute( ILaunchAttributes.PROJECT_NAME, (String)null );
      if (project.getName().equals( thisProject )){
         result.add( configuration );
      }
    }
    return result;
  }

  public void launch( final IResource resource, final IProgressMonitor monitor  )
  throws CoreException {
  if( resource != null ) {
    IProject project = resource.getProject();
    if(ResourceUtil.hasHaskellNature (project) ) {

      ILaunchConfiguration configuration = getConfiguration( project);
      if( configuration != null ) {
        configuration.launch( ILaunchManager.RUN_MODE, monitor );
      }
    }
  }
}

  protected ILaunchConfiguration getConfiguration( final IProject project) throws CoreException {
    List<ILaunchConfiguration> configurations = findConfiguration( project);

    int count = configurations.size();
    ILaunchConfiguration result = null;
    if( count == 1 ) {
      // If there is exactly one config associated with the
      // ICompilationUnit,
      // return it.
      result = configurations.get( 0 );
    } else if (count==0){
      result=createConfiguration( project );
    } else {
      // Otherwise, if there is more than one config associated with the
      // ICompilationUnit, prompt the user to choose one.
      result = choose( configurations );
    }
    return result;
  }

  protected ILaunchConfiguration createConfiguration(final IProject proj )
      throws CoreException {
    ILaunchConfigurationType configType = getConfigType();
    String id = createConfigId( proj.getName()+" test" ); //$NON-NLS-1$
    ILaunchConfigurationWorkingCopy wc = configType.newInstance( null, id );
    wc.setAttribute( ILaunchAttributes.WORKING_DIRECTORY, proj.getLocation().toOSString() );
        //proj.getLocation().append( BWFacade.DIST_FOLDER).toOSString() );
    wc.setAttribute( ILaunchAttributes.PROJECT_NAME, proj.getName() );
    wc.setAttribute( ILaunchAttributes.SYNC_STREAMS, true );
    String s=proj.getLocation().append( BWFacade.DIST_FOLDER_CABAL).toOSString();

    wc.setAttribute( ILaunchAttributes.EXTRA_ARGUMENTS,"test \"--builddir="+s+"\""); //$NON-NLS-1$ //$NON-NLS-2$
    return wc.doSave();
  }


  @Override
  protected String createConfigId( final String name ) {
    //String name = file.getName();
    // FIXME: Remove when Galileo is no longer supported.
    ILaunchManager mgr = getLaunchManager();
    return ILaunchManagerCompat.generateLaunchConfigurationName( mgr, name );
  }
}
