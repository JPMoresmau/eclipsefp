// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.project;

import org.eclipse.core.runtime.CoreException;

/** <p>general proxy for manipulating Cabal package descriptions (as
  * buffer content, not from the file system).</p>
  *
  * @author Leif Frenzel
  */
public interface IManipulateCabalFile {

  public enum Accessor {
    GET_NAME, GET_VERSION, GET_COPYRIGHT, GET_LICENSE, GET_LICENSE_FILE,
    GET_DESCRIPTION, GET_SYNOPSIS, GET_HOMEPAGE, GET_CATEGORY, GET_AUTHOR,
    GET_MAINTAINER, GET_ALL_SOURCE_DIRS
  }

  public enum Mutator {
    SET_NAME, SET_VERSION, SET_COPYRIGHT, SET_LICENSE, SET_LICENSE_FILE,
    SET_DESCRIPTION, SET_SYNOPSIS, SET_HOMEPAGE, SET_CATEGORY, SET_AUTHOR,
    SET_MAINTAINER,
  }

  String get( String buffer, Accessor acc ) throws CoreException;
  String set( String buffer, Mutator mut, String newValue ) throws CoreException;
  String[] getAllSourceDirs( String buffer ) throws CoreException;
}
