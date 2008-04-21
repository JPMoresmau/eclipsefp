// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.util.List;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;

/** <p>interface to the Haskell code that computes folding regions.</p>
  *
  * @author Leif Frenzel
  */
public interface ICodeFolding {

  public interface ICodeFoldingRegion {
    int getStartLine();
    int getEndLine();
  }

  List<ICodeFoldingRegion> performCodeFolding(
    final IContainer srcRoot,
    final IFile file );
}
