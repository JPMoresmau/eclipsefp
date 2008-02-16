// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.hsimpl;

/** <p>enumerates the known types of Haskell implementations.</p>
  *
  * @author Leif Frenzel
  */
public enum HsImplementationType {

  GHC( "ghc", "--numeric-version", "--print-libdir" );  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$

  private String exeCommand;
  private String versionOption;
  private final String libDirOption;

  private HsImplementationType( final String exeCommand,
                                final String versionOption,
                                final String libDirOption ) {
    this.exeCommand = exeCommand;
    this.versionOption = versionOption;
    this.libDirOption = libDirOption;
  }

  String getExecutableCommand() {
    return exeCommand;
  }

  String getVersionOption() {
    return versionOption;
  }

  String getLibDirOption() {
    return libDirOption;
  }
}
