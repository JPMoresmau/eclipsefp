/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.overview;


/**
 * Licenses available by name in a Cabal file.
 * The list is taken from Cabal documentation.
 * @author Alejandro Serrano
 *
 */
public enum License {
  GPL ("GPL", "GNU General Public License"),
  GPL3 ("GPL-3", "GNU General Public License, version 3"),
  AGPL ("AGPL", "GNU Affero General Public License"),
  AGPL3 ("AGPL-3", "GNU Affero General Public License, version 3"),
  GPL2 ("GPL-2", "GNU General Public License, version 2"),
  LGPL ("LGPL", "GNU Lesser General Public License"),
  LGPL3 ("LGPL-3", "GNU Lesser General Public License, version 3"),
  LGPL2 ("LGPL-2.1", "GNU Lesser General Public License, version 2.1"),
  BSD3 ("BSD3", "3-clause BSD license (no advertising clause)","bsd3.txt"),
  BSD4 ("BSD4", "4-clause BSD license (with advertising clause)"),
  Apache2 ("Apache-2", "Apache License, version 2.0"),
  MIT ("MIT", "MIT license"),
  PublicDomain ("PublicDomain", "Public domain"),
  AllRightsReserved ("AllRightsReserved", "All rights reserved "),
  OtherLicense ("OtherLicense", "Other license");

  private String cabalName;
  private String shownName;
  private String fileName;

  License(final String cabalName, final String shownName) {
    this.cabalName = cabalName;
    this.shownName = shownName;
  }

  License(final String cabalName, final String shownName,final String fileName) {
    this.cabalName = cabalName;
    this.shownName = shownName;
    this.fileName = fileName;
  }

  public String getCabalName() {
    return cabalName;
  }

  public String getShownName() {
    return shownName;
  }


  /**
   * @return the fileName
   */
  public String getFileName() {
    return fileName;
  }


}
