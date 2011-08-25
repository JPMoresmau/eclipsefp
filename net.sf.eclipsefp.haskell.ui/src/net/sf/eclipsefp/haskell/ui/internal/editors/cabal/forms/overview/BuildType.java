/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;

/**
 * Types of build that can be used in a Cabal file.
 * @author Alejandro Serrano
 *
 */
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
