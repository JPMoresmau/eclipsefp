// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.configuratorpage;

import net.sf.eclipsefp.common.ui.configurator.IConfiguratorPage;
import net.sf.eclipsefp.common.ui.dialog.ExecutableDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.haskell.ghccompiler.GhcCompilerPlugin;
import net.sf.eclipsefp.haskell.ghccompiler.core.Util;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/** <p>TODO</p>
  *
  * @author Leif Frenzel
  */
public class GHCConfiguratorPage implements IConfiguratorPage, 
                                            IGhcPreferenceNames {

  private String executable;

  public GHCConfiguratorPage() {
    this.executable = getPreferences().getString( EXECUTABLE_NAME );    
  }
  
  // interface methods ofIConfiguratorPage
  ////////////////////////////////////////
  
  public Control createControl( final Composite parent ) {
    String labelText = "GHC executable";
    ExecutableDialogField dlgField = new ExecutableDialogField( parent, 
                                                                labelText ) {
      @Override
      protected String createDisplayContent( final String info ) {
        return Util.queryGHCExecutable( info );
      }
    };
    dlgField.setInfo( executable );
    dlgField.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        executable = ( String )newInfo;
      }
    } );
    return dlgField;
  }
  
  public void performFinish() {
    getPreferences().setValue( EXECUTABLE_NAME, executable );
    GhcCompilerPlugin.getDefault().savePluginPreferences();
  }
  
  public void probed( final Object result ) {
    // unused (no probe registered for this page)
  } 
  
  
  // helping methods
  //////////////////
  
  private Preferences getPreferences() {
    return GhcCompilerPlugin.getDefault().getPluginPreferences();
  }
}
