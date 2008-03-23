// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import org.eclipse.ui.forms.events.IHyperlinkListener;


public interface IFormEntryListener extends IHyperlinkListener {
  void focusGained( FormEntry entry );
  void textDirty( FormEntry entry );
  void textValueChanged( FormEntry entry );
  void browseButtonSelected( FormEntry entry );
  void selectionChanged( FormEntry entry );
}
