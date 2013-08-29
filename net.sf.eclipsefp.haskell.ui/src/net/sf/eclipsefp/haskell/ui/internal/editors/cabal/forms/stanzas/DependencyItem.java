/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

/**
 * Element to be shown in the list of included dependencies.
 * @author Alejandro Serrano
 *
 */
public class DependencyItem {
  private String pkg;
  private String version;

  public DependencyItem(final String pkg, final String version) {
    this.pkg = pkg;
    this.version = version;
  }

  public String getPackage() {
    return pkg;
  }

  public void setPackage( final String pkg ) {
    this.pkg = pkg;
  }

  public String getVersion() {
    return version;
  }

  public void setVersion( final String version ) {
    this.version = version;
  }

  public static DependencyItem fromString(final String value) {
    String trimmed = value.trim();
    int spacePos = trimmed.indexOf( ' ' );
    if (spacePos == -1) {
      return new DependencyItem(trimmed, "");
    } else {
      return new DependencyItem(trimmed.substring( 0, spacePos ), trimmed.substring( spacePos + 1 ).trim());
    }
  }
  @Override
  public String toString() {
    return pkg+(version!=null && version.length()>0?" "+version:"");
  }
}
