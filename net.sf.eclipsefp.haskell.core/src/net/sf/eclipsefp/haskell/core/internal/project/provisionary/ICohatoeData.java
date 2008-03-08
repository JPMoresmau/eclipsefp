// Copyright (c) 2006-2008 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.core.internal.project.provisionary;

import java.util.List;


/** <p>the common interface for data that we can marshall to and from the
  * Haskell side.</p>
  *
  * @author Leif Frenzel
  */
public interface ICohatoeData {

  List<String> marshal();
}
