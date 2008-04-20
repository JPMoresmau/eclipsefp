// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell;

import org.eclipse.core.resources.IFile;

public interface IEditorTextHover {
  String computeInfoHover(
    final IFile cabalFile,
    final IFile file,
    final int line,
    final int column );
}
