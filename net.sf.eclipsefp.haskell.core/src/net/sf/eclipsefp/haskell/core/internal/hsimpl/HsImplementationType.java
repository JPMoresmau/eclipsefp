// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.hsimpl;

/** Known Haskell implementation enumeration.
  *
  * @author Leif Frenzel
  * @author Scott Michel (scottm@aero.org, modifications)
  */
public enum HsImplementationType {
  GHC( "ghc", "--numeric-version", "--print-libdir" );  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$

  private final String exeCommand;
  private final String versionOption;
  private final String libDirOption;

  /** Internal enumeration constructor
   *
   * @param exeCommand The command to execute (binary directory will be prepended)
   * @param versionOption Option flag to query version
   * @param libDirOption Option flag to query library directory(ies)
   */
  private HsImplementationType( final String exeCommand,
                                final String versionOption,
                                final String libDirOption ) {
    this.exeCommand = exeCommand;
    this.versionOption = versionOption;
    this.libDirOption = libDirOption;
  }

  /** Get the Haskell implementation's executable command string */
  String getExecutableCommand() {
    return exeCommand;
  }

  /** Get the Haskell implementation's version option string */
  String getVersionOption() {
    return versionOption;
  }

  /** Get the Haskell implementation's library directory query string */
  String getLibDirOption() {
    return libDirOption;
  }
}
