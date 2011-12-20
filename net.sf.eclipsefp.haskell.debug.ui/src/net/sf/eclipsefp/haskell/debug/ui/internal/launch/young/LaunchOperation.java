// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch.young;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.sf.eclipsefp.haskell.compat.ILaunchManagerCompat;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.young.ILaunchAttributes;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchManager;

/**
 * <p>
 * super class for launch operations. Contains some common functionality.
 * </p>
 *
 * @author Leif Frenzel
 * @author Alejandro Serrano
 * @deprecated
 */
public abstract class LaunchOperation {

  public ILaunchConfiguration[] getConfigurations() throws CoreException {
    return getConfigurations( getConfigType() );
  }

  public static ILaunchConfiguration[] getConfigurations(
      final ILaunchConfigurationType configType ) throws CoreException {
    return getLaunchManager().getLaunchConfigurations( configType );
  }

  public List<ILaunchConfiguration> getConfigurationsForProject(
      final String projectName ) throws CoreException {
    return getConfigurationsForProject( getConfigType(), projectName );
  }

  public static List<ILaunchConfiguration> getConfigurationsForProject(
      final ILaunchConfigurationType type, final String projectName )
      throws CoreException {
    ILaunchConfigurationType configType = type;
    ILaunchConfiguration[] configs = getLaunchManager()
        .getLaunchConfigurations( configType );
    List<ILaunchConfiguration> ret = new ArrayList<ILaunchConfiguration>();
    for( ILaunchConfiguration config: configs ) {
      if( projectName.equals( getProjectName( config ) ) ) {
        ret.add( config );
      }
    }
    return ret;
  }

  public static List<ILaunchConfiguration> getConfigurationsForStanza(
      final ILaunchConfigurationType type, final String projectName,
      final String stanzaName ) throws CoreException {
    List<ILaunchConfiguration> ret = new ArrayList<ILaunchConfiguration>();
    for( ILaunchConfiguration config: getConfigurationsForProject( type,
        projectName ) ) {
      if( stanzaName.equals( getStanzaName( config ) ) ) {
        ret.add( config );
      }
    }
    return ret;
  }

  public static List<ILaunchConfiguration> getConfigurationsForFiles(
      final ILaunchConfigurationType type, final String projectName,
      final Collection<String> elements ) throws CoreException {
    List<ILaunchConfiguration> ret = new ArrayList<ILaunchConfiguration>();
    for( ILaunchConfiguration config: getConfigurationsForProject( type,
        projectName ) ) {
      Set<String> askedElements = new HashSet<String>(elements);
      Set<String> compElements = new HashSet<String>(getFileNames( config ));
      if( askedElements.equals( compElements ) ) {
        ret.add( config );
      }
    }
    return ret;
  }

  public static ILaunchManager getLaunchManager() {
    return DebugPlugin.getDefault().getLaunchManager();
  }

  public static String createConfigId( final String name ) {
    ILaunchManager mgr = getLaunchManager();
    // FIXME: Remove when Galileo is no longer supported.
    return ILaunchManagerCompat.generateLaunchConfigurationName( mgr,
        name.replace( '/', '.' ) );
  }

  public final ILaunchConfigurationType getConfigType() {
    return getConfigType( getConfigTypeName() );
  }

  public static final ILaunchConfigurationType getConfigType(
      final String configTypeName ) {
    return getLaunchManager().getLaunchConfigurationType( configTypeName );
  }

  protected abstract String getConfigTypeName();

  public static String getProjectName( final ILaunchConfiguration configuration )
      throws CoreException {
    String att = ILaunchAttributes.PROJECT_NAME;
    return configuration.getAttribute( att, ILaunchAttributes.EMPTY );
  }

  public static String getStanzaName( final ILaunchConfiguration configuration )
      throws CoreException {
    String att = ILaunchAttributes.STANZA;
    return configuration.getAttribute( att, ILaunchAttributes.EMPTY );
  }

  @SuppressWarnings("unchecked")
  public static List<String> getFileNames( final ILaunchConfiguration configuration )
      throws CoreException {
    String att = ILaunchAttributes.FILES;
    return configuration.getAttribute( att, new ArrayList<String>() );
  }

  // helping methods
  // ////////////////

  /*private Shell getActiveShell() {
    IWorkbench workbench = HaskellUIPlugin.getDefault().getWorkbench();
    return workbench.getActiveWorkbenchWindow().getShell();
  }*/
}
