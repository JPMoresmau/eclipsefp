// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.configuratorpage;

import net.sf.eclipsefp.common.ui.configurator.IConfiguratorPage;
import net.sf.eclipsefp.common.ui.dialog.ExecutableDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.haskell.haddock.HaddockPlugin;
import net.sf.eclipsefp.haskell.haddock.core.HaddockUtil;
import net.sf.eclipsefp.haskell.haddock.core.preferences.IHaddockPreferenceNames;

import org.eclipse.core.runtime.Preferences;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

/** <p>configurator page for the Haddock executable.</p>
  *
  * @author Leif Frenzel
  */
public class HaddockConfiguratorPage implements IConfiguratorPage,
                                                IHaddockPreferenceNames {

  private String executable;
  private ExecutableDialogField dlgField;

  public HaddockConfiguratorPage() {
    this.executable = getPreferences().getString( EXECUTABLE_NAME );
  }
    
  // interface methods of IConfiguratorPage
  /////////////////////////////////////////
  
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 1, false ) );
    
    createExecutableDialogField( composite );
    
    Label label = new Label( composite, SWT.WRAP );
    String infoText =   "Press the button below to have the Haddock executable "
                      + "automatically detected.\nIf nothing has been found "
                      + "during the probing, you have to set the executable\n"
                      + "manually.";
    label.setText( infoText );
    GridData gridData = new GridData( GridData.FILL_BOTH );
    gridData.verticalAlignment = GridData.VERTICAL_ALIGN_END;
    label.setLayoutData( gridData );
    
    return composite;
  }
  
  public void performFinish() {
    getPreferences().setValue( EXECUTABLE_NAME, executable );
    HaddockPlugin.getDefault().savePluginPreferences();
  }

  public void probed( final Object result ) {
    if( result != null ) {
      dlgField.setInfo( result );
      executable = ( String )result;  
    }
  } 

  
  // UI creation methods
  //////////////////////
  
  private void createExecutableDialogField( final Composite parent ) {
    String labelText = "Haddock executable";
    dlgField = new ExecutableDialogField( parent, labelText ) {
      @Override
      protected String createDisplayContent( final String info ) {
        return HaddockUtil.queryHaddockExecutable( info );
      }
    };
    dlgField.setInfo( executable );
    dlgField.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        executable = ( String )newInfo;
      }
    } );
    dlgField.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
  }

  
  // helping methods
  //////////////////
  
  private Preferences getPreferences() {
    return HaddockPlugin.getDefault().getPluginPreferences();
  }
}
