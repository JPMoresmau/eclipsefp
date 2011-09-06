/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

/**
 * Listener for changes in a form entry in a managed form.
 * @author Alejandro Serrano
 *
 */
public interface IFormEntryListener {
  void focusGained( FormEntry entry );
  void textDirty( FormEntry entry );
  void textValueChanged( FormEntry entry );
  void selectionChanged( FormEntry entry );
}
