/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import java.io.IOException;
import net.sf.eclipsefp.haskell.style.stylishhaskell.SHConfiguration;
import net.sf.eclipsefp.haskell.style.stylishhaskell.StylishHaskell;
import net.sf.eclipsefp.haskell.style.stylishhaskell.ui.SHConfigurationComposite;
import net.sf.eclipsefp.haskell.style.util.StyleText;
import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;


/**
 * Configure path to stylish-haskell
 * @author JP Moresmau
 *
 */
public class StylishHaskellPP extends ExecutablePP {
  private SHConfigurationComposite confComp;

  /**
   * format on save?
   */
  private BooleanFieldEditor formatOnSave;

  public StylishHaskellPP(){
    super("stylish-haskell","stylish-haskell",IPreferenceConstants.STYLISHHASKELL_EXECUTABLE);
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ExecutablePP#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parentComposite ) {
    Control c=super.createContents( parentComposite );

    confComp=new SHConfigurationComposite( parentComposite, SWT.NONE );
    confComp.setConfiguration( StylishHaskell.getWorkspaceConfiguration() );

    formatOnSave = new BooleanFieldEditor( IPreferenceConstants.STYLISHHASKELL_SAVE,
        UITexts.sh_save,
        parentComposite );
    formatOnSave.setPage(this);
    formatOnSave.setPreferenceStore( getPreferenceStore() );
    formatOnSave.load();

    return c;
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ExecutablePP#performOk()
   */
  @Override
  public boolean performOk() {
    SHConfiguration conf=confComp.getConfiguration();
    try {
      StylishHaskell.setWorkspaceConfiguration( conf );
      formatOnSave.store();
      return super.performOk();
    } catch (IOException ioe){
      MessageDialog.openError( getShell(), StyleText.sh_save_error, ioe.getLocalizedMessage() );
      return false;
    }
  }

  /* (non-Javadoc)
   * @see org.eclipse.jface.preference.PreferencePage#performDefaults()
   */
  @Override
  protected void performDefaults() {
    confComp.setConfiguration(new SHConfiguration());
    formatOnSave.loadDefault();
    super.performDefaults();
  }
}

