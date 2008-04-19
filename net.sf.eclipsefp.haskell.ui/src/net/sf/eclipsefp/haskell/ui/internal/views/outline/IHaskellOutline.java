// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.outline;

import java.util.List;
import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;

/** <p>interface for calling the Haskell code that computes the Outline
  * for Haskell source files.</p>
  *
  * @author Leif Frenzel
  */
public interface IHaskellOutline {
  List<ITreeElement> computeOutline( String buffer );
}
