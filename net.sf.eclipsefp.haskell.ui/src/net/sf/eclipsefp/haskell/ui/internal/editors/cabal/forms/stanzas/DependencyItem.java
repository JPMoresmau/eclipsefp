package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;


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
    int spacePos = value.trim().indexOf( ' ' );
    if (spacePos == -1) {
      return new DependencyItem(value, "");
    } else {
      return new DependencyItem(value.substring( 0, spacePos ), value.substring( spacePos + 1 ));
    }
  }
}
