// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.internal.code;

import de.leiffrenzel.cohatoe.server.core.CohatoeException;

/** <p>interface to the OrganizeImports function.</p>
  *
  * @author Leif Frenzel
  */
public interface IOrganizeImports {
  String organizeImports( String buffer ) throws CohatoeException;
}
