/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.backend;


import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * The dialog telling the user that she's missing some executables, and giving the option to install them
 * @author JP Moresmau
 *
 */
public class InstallExecutableDialog extends Dialog {
  protected boolean buildWrapper=true;
  protected boolean scionBrowser=true;
  protected String buildWrapperMinVersion="";
  protected String scionBrowserMinVersion="";

  private Button bUser;
  private Button bIgnore;

  private Button bExtras;

  public InstallExecutableDialog( final Shell parentShell,final boolean buildWrapper, final String buildWrapperMinVersion,
                                                          final boolean scionBrowser, final String scionBrowserMinVersion) {
    super( parentShell );
    this.buildWrapper=buildWrapper;
    this.scionBrowser=scionBrowser;
    this.buildWrapperMinVersion = buildWrapperMinVersion;
    this.scionBrowserMinVersion = scionBrowserMinVersion;
  }



  @Override
  protected void createButtonsForButtonBar( final Composite parent ) {
    super.createButtonsForButtonBar( parent );
    getButton( OK ).setText( UITexts.executablesmissing_install );
  }

  protected String getTitle(){
    return UITexts.executablesmissing_title;
  }

  protected String getMessage1(){
    return UITexts.executablesmissing_message1;
  }

  protected String getMessage2(){
    return UITexts.executablesmissing_message2;
  }

  protected String getMessageText(){
    if (buildWrapper){
      if (scionBrowser){
        String[] bindings = {"buildwrapper", buildWrapperMinVersion, "scion-browser", scionBrowserMinVersion};
        return NLS.bind( getMessage2(), bindings );
      } else {
        return NLS.bind( getMessage1(), "buildwrapper", buildWrapperMinVersion);
      }
    } else {
      return NLS.bind( getMessage1(), "scion-browser", scionBrowserMinVersion);
    }
  }

  @Override
  protected void configureShell( final Shell newShell ) {
    super.configureShell( newShell );
    newShell.setText( getTitle() );
    newShell.setImage( HaskellUIImages.getImage( IImageNames.HASKELL_MISC ) );
  }

  @Override
  protected Control createDialogArea( final Composite parent ) {
    Composite c=(Composite)super.createDialogArea( parent );
    ((GridLayout)c.getLayout()).numColumns=2;

    Label l=new Label(c,SWT.NONE);
    l.setText( getMessageText() );
    GridData gd=new GridData(GridData.FILL_HORIZONTAL);
    gd.horizontalSpan=2;
    l.setLayoutData( gd );

    bExtras=new Button(c,SWT.CHECK);
    bExtras.setText( UITexts.executables_extra );
    bExtras.setSelection( true );
    gd=new GridData(GridData.FILL_HORIZONTAL);
    gd.horizontalSpan=2;
    bExtras.setLayoutData( gd );

    bUser=new Button(c,SWT.CHECK);
    bUser.setText( UITexts.executablesmissing_user );
    bUser.setSelection( true );

    bIgnore=new Button(c,SWT.CHECK);
    bIgnore.setText( UITexts.executablesmissing_ignore);



    return c;
  }

  protected String getIgnorePreference(){
    return IPreferenceConstants.IGNORE_MISSING_EXECUTABLE;
  }

  private void setIgnoreFlag(){
    IPreferenceStore prefs = HaskellUIPlugin.getDefault().getPreferenceStore();
    prefs.setValue( getIgnorePreference(), bIgnore.getSelection() );

  }

  @Override
  public boolean close() {
    setIgnoreFlag();
    return super.close();
  }

  @Override
  protected void okPressed() {
    final InstallExecutableRunnable j=new InstallExecutableRunnable();
    if (buildWrapper){
      j.getPackages().add( new InstallExecutableRunnable.Package( "buildwrapper", IPreferenceConstants.BUILDWRAPPER_EXECUTABLE) );
    }
    if (scionBrowser){
      j.getPackages().add( new InstallExecutableRunnable.Package( "scion-browser", IPreferenceConstants.SCION_BROWSER_SERVER_EXECUTABLE) );
    }
    if (bExtras.getSelection()){
      j.getPackages().add( new InstallExecutableRunnable.Package( "hoogle", IPreferenceConstants.SCION_BROWSER_EXTRA_HOOGLE_PATH) );
      j.getPackages().add( new InstallExecutableRunnable.Package( "hlint", IPreferenceConstants.HLINT_EXECUTABLE) );
      j.getPackages().add( new InstallExecutableRunnable.Package( "stylish-haskell",IPreferenceConstants.STYLISHHASKELL_EXECUTABLE) );
      j.getPackages().add( new InstallExecutableRunnable.Package( "SourceGraph",IPreferenceConstants.SOURCEGRAPH_EXECUTABLE) );
    }
    //j.setBuildWrapper( buildWrapper );
    j.setCabalUpdate( true );
    j.setGlobal( !bUser.getSelection() );
    //j.setScionBrowser( scionBrowser );
    new Thread(j).start();
    super.okPressed();
  }

}
