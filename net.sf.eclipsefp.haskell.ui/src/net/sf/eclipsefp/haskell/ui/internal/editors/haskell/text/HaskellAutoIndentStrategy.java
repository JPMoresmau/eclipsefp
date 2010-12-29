// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import org.eclipse.jface.text.DefaultIndentLineAutoEditStrategy;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IDocument;


/** <p>the auto indent strategy for the Haskell editor (insert spaces for
  * tabs, etc.).</p>
  *
  * @author Leif Frenzel
  */
public class HaskellAutoIndentStrategy extends DefaultIndentLineAutoEditStrategy {
  // Override methods DefaultIndentLineAutoEditStrategy
  ///////////////////////////////////////////

  @Override
  public void customizeDocumentCommand( final IDocument document,
                                        final DocumentCommand command ) {
    // FIXME: Help with layout mode here:
    super.customizeDocumentCommand( document, command );
  }
}