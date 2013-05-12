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
 * Helper preference page for yesod
 * @author JP Moresmau
 *
 */
public class YesodPP extends ExecutablePP {
  /**
   * build with cabal dev?
   */
  private BooleanFieldEditor cabalDevField;

  public YesodPP(){
    /**
     * from yesod 1.2, the executable is still yesod but the project is yesod-bin
     */
    super("Yesod","yesod","yesod-bin",IPreferenceConstants.YESOD_EXECUTABLE);
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ExecutablePP#createContents(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createContents( final Composite parentComposite ) {
    super.createContents( parentComposite );
    cabalDevField = new BooleanFieldEditor( IPreferenceConstants.YESOD_CABALDEV,
        UITexts.yesod_cabaldev,
        parentComposite );
    cabalDevField.setPage(this);
    cabalDevField.setPreferenceStore( getPreferenceStore() );
    cabalDevField.load();

    return parentComposite;
  }

  /* (non-Javadoc)
   * @see net.sf.eclipsefp.haskell.ui.internal.preferences.scion.ExecutablePP#performOk()
   */
  @Override
  public boolean performOk() {
    cabalDevField.store();
    return super.performOk();
  }
}
