// Copyright (c) 2003-2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.debug.ui.internal.launch;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.HaskellLaunchDelegate;
import net.sf.eclipsefp.haskell.debug.core.internal.launch.ILaunchAttributes;
import net.sf.eclipsefp.haskell.debug.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;

/** <p>super class for launch operations. Contains some common
  * functionality.</p>
  *
  * @author Leif Frenzel
  */
public abstract class LaunchOperation {

  static final String CONFIG_TYPE = HaskellLaunchDelegate.class.getName();

  public static ILaunchConfiguration[] getConfigurations() throws CoreException {
    ILaunchConfigurationType configType = getConfigType();
    return getLaunchManager().getLaunchConfigurations( configType );
  }

  public static List<ILaunchConfiguration> getConfigurationsForProject(final String projectName) throws CoreException {
    ILaunchConfigurationType configType = getConfigType();
    ILaunchConfiguration[] configs=getLaunchManager().getLaunchConfigurations( configType );
    List<ILaunchConfiguration> ret=new ArrayList<ILaunchConfiguration>();
    for (ILaunchConfiguration config:configs){
      if (projectName.equals(getProjectName(config))){
        ret.add(config);
      }
    }
    return ret;
  }


  public static ILaunchManager getLaunchManager() {
    return DebugPlugin.getDefault().getLaunchManager();
  }

  String createConfigId( final String name ) {
    return getLaunchManager().generateUniqueLaunchConfigurationNameFrom( name.replace( '/', '.' ) );
  }

  public static ILaunchConfigurationType getConfigType() {
    return getLaunchManager().getLaunchConfigurationType( CONFIG_TYPE );
  }

  public static String getProjectName( final ILaunchConfiguration configuration )
                                                          throws CoreException {
    String att = ILaunchAttributes.PROJECT_NAME;
    return configuration.getAttribute( att, ILaunchAttributes.EMPTY );
  }

  String getExePath( final ILaunchConfiguration config ) throws CoreException {
    String att = ILaunchAttributes.EXECUTABLE;
    return config.getAttribute( att, ILaunchAttributes.EMPTY );
  }

  ILaunchConfiguration choose( final List<ILaunchConfiguration> configs ) {
    IDebugModelPresentation pres = DebugUITools.newDebugModelPresentation();
    Shell shell = getActiveShell();
    ElementListSelectionDialog dialog = new ElementListSelectionDialog( shell,
        pres );
    dialog.setElements( configs.toArray() );
    dialog.setTitle( UITexts.launchOperation_title );
    dialog.setMessage( UITexts.launchOperation_msg );
    dialog.setMultipleSelection( false );
    ILaunchConfiguration result = null;
    int returnVal = dialog.open();
    pres.dispose();
    if( returnVal == Window.OK ) {
      result = ( ILaunchConfiguration )dialog.getFirstResult();
    }
    return result;
  }


  // helping methods
  // ////////////////

  private Shell getActiveShell() {
    IWorkbench workbench = HaskellUIPlugin.getDefault().getWorkbench();
    return workbench.getActiveWorkbenchWindow().getShell();
  }
}
