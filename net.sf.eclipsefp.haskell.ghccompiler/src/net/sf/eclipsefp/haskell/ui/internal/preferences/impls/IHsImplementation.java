// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.preferences.impls;


/** <p>represents an installed Haskell implementation.</p>
  *
  * @author Leif Frenzel
  */
public interface IHsImplementation {

  HsImplementationType getType();
  /** <p>displayed to the user for informational purposes. May be empty.</p> */
  String getVersion();
  /** <p>returns the name of this installed Haskell implementation. This
    * name is supplied by the user and is displayed to the user to identify
    * this installed implementation.</p> */
  String getName();
  String getBinDir();
}
