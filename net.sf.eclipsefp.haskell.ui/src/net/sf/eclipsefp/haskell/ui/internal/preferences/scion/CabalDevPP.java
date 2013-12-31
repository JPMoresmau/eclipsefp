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
import org.eclipse.swt.SWT;
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

    uniqueSandboxField= new BooleanFieldEditor( IPreferenceConstants.UNIQUE_SANDBOX,
        UITexts.executables_preferences_unique_sandbox,
        parentComposite );
    uniqueSandboxField.setPage(this);
    uniqueSandboxField.setPreferenceStore( getPreferenceStore() );
    uniqueSandboxField.load();

    return c;
  }

  @Override
  public boolean performOk() {
    uniqueSandboxField.store();
    cabalSandboxField.store();
    return super.performOk();
  }

}
