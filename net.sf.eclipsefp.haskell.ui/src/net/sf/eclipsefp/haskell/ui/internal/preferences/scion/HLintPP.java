/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;


/**
 * Helper preference page for HLint
 * @author JP Moresmau
 *
 */
public class HLintPP extends ExecutablePP {
  /**
   * always display suggestion?
   */
  private BooleanFieldEditor alwaysFullField;

  public HLintPP() {
    super( "HLint" , "hlint", IPreferenceConstants.HLINT_EXECUTABLE );
  }


  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ExecutablePP#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parentComposite ) {
    super.createContents( parentComposite );
    alwaysFullField = new BooleanFieldEditor( IPreferenceConstants.HLINT_ALWAYS_SHOW_FULL_TEXT,
        UITexts.hlint_preferences_full,
        parentComposite );
    alwaysFullField.setPage(this);
    alwaysFullField.setPreferenceStore( getPreferenceStore() );
    alwaysFullField.load();

    return parentComposite;
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ExecutablePP#performOk()
   */
  @Override
  public boolean performOk() {
    alwaysFullField.store();
    return super.performOk();
  }
}
