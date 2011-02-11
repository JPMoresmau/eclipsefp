// Copyright (c) 2008 by Leif Frenzel. All rights reserved.
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.text;

import net.sf.eclipsefp.haskell.scion.types.Occurrence;


/** <p>implementors know how to compute occurrences of the identifier (if any)
  * at a cursor position in a Haskell source text.</p>
  *
  * @author Leif Frenzel
  */
public interface IMarkOccurrences {

  Occurrence[] mark( final String bufferContent,
                     final int cursorLine,
                     final int cursorColumn );
}
