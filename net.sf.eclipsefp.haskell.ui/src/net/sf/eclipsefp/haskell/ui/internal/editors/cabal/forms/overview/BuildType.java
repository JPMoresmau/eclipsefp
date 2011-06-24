/**
 * (c) 2011, Alejandro Serrano
 * Released under the condidtions of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;


public enum BuildType {
  Simple ("Simple", "Simple"),
  Configure ("Configure", "Configure"),
  Make ("Make", "Make"),
  Custom ("Custom", "Custom (user-supplied Setup.hs)");

  String cabalName;
  String shownName;

  BuildType(final String cabalName, final String shownName) {
    this.cabalName = cabalName;
    this.shownName = shownName;
  }

  public String getCabalName() {
    return cabalName;
  }

  public String getShownName() {
    return shownName;
  }
}
