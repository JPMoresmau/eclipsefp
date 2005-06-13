// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.launch;

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.*;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugModelPresentation;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;

import de.leiffrenzel.fp.haskell.core.launch.HaskellLaunchDelegate;
import de.leiffrenzel.fp.haskell.core.launch.ILaunchAttributes;
import de.leiffrenzel.fp.haskell.ui.HaskellUIPlugin;

/** <p>super class for launch operations. Contains some common 
  * functionality.</p>
  * 
  * @author Leif Frenzel
  */
abstract class LaunchOperation {

  static final String CONFIG_TYPE = HaskellLaunchDelegate.class.getName();

  ILaunchConfiguration[] getConfigurations() throws CoreException {
    ILaunchConfigurationType configType = getConfigType();
    return getLaunchManager().getLaunchConfigurations( configType );
  }

  ILaunchManager getLaunchManager() {
    return DebugPlugin.getDefault().getLaunchManager();
  }
      
  String createConfigId( final String name ) {
    return getLaunchManager().generateUniqueLaunchConfigurationNameFrom( name );
  }
      
  ILaunchConfigurationType getConfigType() {
    return getLaunchManager().getLaunchConfigurationType( CONFIG_TYPE );
  }
  
  String getProjectName( final ILaunchConfiguration configuration )
                                                          throws CoreException {
    return configuration.getAttribute( ILaunchAttributes.PROJECT_NAME, "" );
  }

  String getExePath( final ILaunchConfiguration config ) throws CoreException {
    return config.getAttribute( ILaunchAttributes.EXECUTABLE, "" );
  }
  
  ILaunchConfiguration chooseConfiguration( final List configs ) {
    IDebugModelPresentation pres = DebugUITools.newDebugModelPresentation();
    Shell shell = getActiveShell();
    ElementListSelectionDialog dialog = new ElementListSelectionDialog( shell, 
                                                                        pres );
    dialog.setElements( configs.toArray() );
    dialog.setTitle( "Select Launch Configuration" );
    dialog.setMessage( "Select a launch configuration to run" );
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
  //////////////////
  
  private Shell getActiveShell() {
    IWorkbench workbench = HaskellUIPlugin.getDefault().getWorkbench();
    return workbench.getActiveWorkbenchWindow().getShell();
  }
}
