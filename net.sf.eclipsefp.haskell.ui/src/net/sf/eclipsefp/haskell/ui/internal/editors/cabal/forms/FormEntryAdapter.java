// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.forms.events.HyperlinkEvent;

public class FormEntryAdapter implements IFormEntryListener {

  private final IActionBars actionBars;

  public FormEntryAdapter() {
    this.actionBars = null;
  }

	public FormEntryAdapter( final IActionBars actionBars ) {
		this.actionBars = actionBars;
	}


	// interface methods of IFormEntryListener
	//////////////////////////////////////////

	public void focusGained( final FormEntry entry ) {
	  // unimplemented, subclasses override
	}

	public void textDirty( final FormEntry entry ) {
	  // unimplemented, subclasses override
	}

	public void textValueChanged( final FormEntry entry ) {
	  // unimplemented, subclasses override
	}

	public void browseButtonSelected( final FormEntry entry ) {
	  // unimplemented, subclasses override
	}

	public void selectionChanged( final FormEntry entry ) {
	  // unimplemented, subclasses override
	}


	// interface methods of IHyperLinkListener
  // ////////////////////////////////////////

  public void linkActivated( final HyperlinkEvent evt ) {
    // unimplemented, subclasses override
  }

  public void linkEntered( final HyperlinkEvent evt ) {
    if( actionBars != null ) {
      IStatusLineManager mng = actionBars.getStatusLineManager();
      mng.setMessage( evt.getLabel() );
    }
  }

  public void linkExited( final HyperlinkEvent evt ) {
    if( actionBars != null ) {
      IStatusLineManager mng = actionBars.getStatusLineManager();
      mng.setMessage( null );
    }
  }
}
