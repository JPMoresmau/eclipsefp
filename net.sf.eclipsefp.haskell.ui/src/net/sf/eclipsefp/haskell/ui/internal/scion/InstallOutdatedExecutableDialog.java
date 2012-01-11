/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.scion;

import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.widgets.Shell;

/**
 * Install executables when they are too old
 * @author JP Moresmau
 *
 */
public class InstallOutdatedExecutableDialog extends InstallExecutableDialog {

  public InstallOutdatedExecutableDialog( final Shell parentShell,
      final boolean buildWrapper, final boolean scionBrowser ) {
    super( parentShell, buildWrapper, scionBrowser );
  }

  @Override
  protected String getMessage1() {
    return UITexts.executablestoo_old_message1;
  }

  @Override
  protected String getMessage2() {
    return UITexts.executablestoo_old_message2;
  }

  @Override
  protected String getTitle() {
    return UITexts.executablestoo_old_title;
  }

  @Override
  protected String getIgnorePreference() {
    return IPreferenceConstants.IGNORE_TOOOLD_EXECUTABLE;
  }

}
