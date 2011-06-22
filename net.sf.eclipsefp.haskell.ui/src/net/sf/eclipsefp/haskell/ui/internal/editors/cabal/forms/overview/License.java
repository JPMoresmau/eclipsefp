package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;


public enum License {
  GPL3 ("GPL-3", "GNU General Public License, version 3"),
  GPL2 ("GPL-2", "GNU General Public License, version 2"),
  LGPL3 ("LGPL-3", "GNU Lesser General Public License, version 3"),
  LGPL2 ("LGPL-2.1", "GNU Lesser General Public License, version 2.1"),
  BSD3 ("BSD3", "3-clause BSD license (no advertising clause)"),
  BSD4 ("BSD4", "4-clause BSD license (with advertising clause)"),
  MIT ("MIT", "MIT license"),
  PublicDomain ("PublicDomain", "Public domain"),
  AllRightsReserved ("AllRightsReserved", "All rights reserved "),
  OtherLicense ("OtherLicense", "Other license");

  String cabalName;
  String shownName;

  License(final String cabalName, final String shownName) {
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
