// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.ui.configuratorpage;

import net.sf.eclipsefp.common.ui.configurator.IConfiguratorPage;
import net.sf.eclipsefp.common.ui.dialog.FileDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.haskell.hugs.HugsPlugin;
import net.sf.eclipsefp.haskell.hugs.core.preferences.IHugsPreferenceNames;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/** <p>configurator wizard page to simplify the setting of the HUGS executable
  * for the user.</p>
  *
  * @author Leif Frenzel
  */
public class HugsConfiguratorPage implements IConfiguratorPage, 
                                             IHugsPreferenceNames {

  private String executable;

  public HugsConfiguratorPage() {
    this.executable = getPreferences().getString( EXECUTABLE_NAME );
  }

  
  // interface methods of IConfiguratorPage
  /////////////////////////////////////////
  
  public Control createControl( final Composite parent ) {
    String[] extensions = new String[] { "*" };
    String text = "HUGS executable: ";
    FileDialogField dlgField = new FileDialogField( parent, text, extensions );
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
    HugsPlugin.getDefault().savePluginPreferences();
  }

  public void probed( final Object result ) {
    // unused (no probe registered for this page)
  } 

  
  // helping methods
  //////////////////
  
  private Preferences getPreferences() {
    return HugsPlugin.getDefault().getPluginPreferences();
  }
}
