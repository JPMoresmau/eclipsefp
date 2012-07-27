/**
 * Copyright (c) 2012 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.ui.internal.scion;

import net.sf.eclipsefp.haskell.ui.internal.preferences.IPreferenceConstants;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Shell;

/**
 * Install executables when they are too old
 * @author JP Moresmau
 *
 */
public class InstallOutdatedExecutableDialog extends InstallExecutableDialog {
  protected String buildWrapperActualVersion="";
  protected String buildWrapperPath="";
  protected String scionBrowserActualVersion="";
  protected String scionBrowserPath="";

  public InstallOutdatedExecutableDialog( final Shell parentShell,
      final boolean buildWrapper, final String buildWrapperMinVersion, final String buildWrapperActualVersion, final String buildWrapperPath,
      final boolean scionBrowser, final String scionBrowserMinVersion, final String scionBrowserActualVersion, final String scionBrowserPath ) {
    super( parentShell, buildWrapper, buildWrapperMinVersion, scionBrowser, scionBrowserMinVersion );
    this.buildWrapperActualVersion = buildWrapperActualVersion;
    this.buildWrapperPath = buildWrapperPath;
    this.scionBrowserActualVersion = scionBrowserActualVersion;
    this.scionBrowserPath = scionBrowserPath;
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
  protected String getMessageText(){
    if (this.buildWrapper){
      if (this.scionBrowser){
        String[] bindings = {"buildwrapper",buildWrapperMinVersion, buildWrapperActualVersion, buildWrapperPath,
                             "scion-browser",scionBrowserMinVersion, scionBrowserActualVersion, scionBrowserPath};
        return NLS.bind( getMessage2(), bindings);
      } else {
        String[] bindings = {"buildwrapper", buildWrapperMinVersion, buildWrapperActualVersion, buildWrapperPath};
        return NLS.bind( getMessage1(), bindings);
      }
    } else {
      String[] bindings = {"scion-browser", scionBrowserMinVersion, scionBrowserActualVersion, scionBrowserPath};
      return NLS.bind( getMessage1(), bindings);
    }
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
