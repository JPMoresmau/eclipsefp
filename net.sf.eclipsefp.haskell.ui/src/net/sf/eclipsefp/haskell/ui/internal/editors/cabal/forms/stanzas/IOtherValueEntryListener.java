package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.stanzas;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms.FormEntry;

/**
 * Listeners for form entries which have more than one value exposed.
 * @author Alejandro Serrano
 *
 */
public interface IOtherValueEntryListener {
  void otherTextValueChanged( FormEntry entry );
}
