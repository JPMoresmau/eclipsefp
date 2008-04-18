// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.views.common;

import java.util.List;

/** <p>generic tree object that can be represented on Eclipse's viewers
  * with the help of TreeElementCP and TreeElementLP.</p>
  *
  * @author Leif Frenzel
  */
public interface ITreeElement {

  List<?> getChildren();
  Object getParent();
  String getText();
  String getImageKey();
}
