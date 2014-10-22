/**
 * Copyright (c) 2013 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import net.sf.eclipsefp.haskell.core.cabal.CabalImplementation;
import net.sf.eclipsefp.haskell.core.cabal.CabalImplementationManager;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;


/**
 * Preference page for cabal-dev
 * @author JP Moresmau
 *
 */
public class CabalDevPP extends ExecutablePP {
  private BooleanFieldEditor uniqueSandboxField;

  private BooleanFieldEditor cabalSandboxField;

  private DirectoryFieldEditor uniqueSandboxLocationField;

  private BooleanFieldEditor manageDependenciesField;

  public CabalDevPP(){
    super("cabal-dev","cabal-dev",IPreferenceConstants.CABALDEV_EXECUTABLE);
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ExecutablePP#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parentComposite ) {
    Control c=super.createContents( parentComposite );

    Label l=new Label(parentComposite,SWT.NONE);
    l.setText( UITexts.preferences_cabaldev_note);

    cabalSandboxField= new BooleanFieldEditor( IPreferenceConstants.CABAL_SANDBOX,
        UITexts.executables_preferences_cabal_sandbox,
        parentComposite );
    cabalSandboxField.setPage(this);
    cabalSandboxField.setPreferenceStore( getPreferenceStore() );
    cabalSandboxField.load();
    CabalImplementation impl=CabalImplementationManager.getInstance().getDefaultCabalImplementation();
    if (impl==null || !impl.allowsSandbox()){
      cabalSandboxField.setEnabled( false, parentComposite );
    }
    cabalSandboxField.fillIntoGrid( parentComposite, 3 );


    uniqueSandboxField= new BooleanFieldEditor( IPreferenceConstants.UNIQUE_SANDBOX,
        UITexts.executables_preferences_unique_sandbox,
        parentComposite );
    uniqueSandboxField.setPage(this);
    uniqueSandboxField.setPreferenceStore( getPreferenceStore() );
    uniqueSandboxField.load();
    uniqueSandboxField.fillIntoGrid( parentComposite, 3 );


    final Composite locComposite=new Composite(parentComposite,SWT.NONE);
    GridData gd=new GridData(SWT.FILL,SWT.CENTER,true,false);
    gd.horizontalSpan=3;
    locComposite.setLayoutData( gd );
    locComposite.setLayout( new GridLayout(3,false) );


    uniqueSandboxLocationField=new DirectoryFieldEditor( IPreferenceConstants.UNIQUE_SANDBOX_PATH, UITexts.executables_preferences_unique_sandbox_location, locComposite );
    uniqueSandboxLocationField.setPage(this);
    uniqueSandboxLocationField.setPreferenceStore( getPreferenceStore() );
    uniqueSandboxLocationField.load();

    uniqueSandboxLocationField.setEnabled( uniqueSandboxField.getBooleanValue(), locComposite );
    uniqueSandboxField.setPropertyChangeListener( new IPropertyChangeListener() {

      @Override
      public void propertyChange( final PropertyChangeEvent event ) {
        uniqueSandboxLocationField.setEnabled( uniqueSandboxField.getBooleanValue(), locComposite );
      }
    } );
    uniqueSandboxLocationField.fillIntoGrid( locComposite, 3 );

    manageDependenciesField= new BooleanFieldEditor( IPreferenceConstants.MANAGE_DEPENDENCIES,
        UITexts.executables_preferences_manage_dependencies,
        parentComposite );
    manageDependenciesField.setPage(this);
    manageDependenciesField.setPreferenceStore( getPreferenceStore() );
    manageDependenciesField.load();
    manageDependenciesField.fillIntoGrid( parentComposite, 3 );



    getShell().layout(true,true);

    return c;
  }


  @Override
  public boolean performOk() {
    uniqueSandboxField.store();
    cabalSandboxField.store();
    uniqueSandboxLocationField.store();
    manageDependenciesField.store();
    return super.performOk();
  }

}
