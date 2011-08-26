/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.forms;

import java.util.ArrayList;
import net.sf.eclipsefp.haskell.core.cabalmodel.CabalSyntax;
import org.eclipse.core.resources.IProject;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;

/**
 * Common class for Cabal entry widgets, which provide common functionality
 * where the editor pages can hook on.
 * @author Alejandro Serrano
 */
public abstract class FormEntry {

  private CabalSyntax property;
  private final ArrayList<IFormEntryListener> listeners = new ArrayList<IFormEntryListener>();

  public abstract void init( final IProject project,
      final Composite parent, final FormToolkit toolkit, final int style );

  public abstract Control getControl();

  public abstract int heightHint();

  public abstract void setValue( String value, boolean blockNotification );

  public abstract String getValue();

  public abstract void setEditable( boolean editable );

  public void setProperty( final CabalSyntax property ) {
    this.property = property;
  }

  public CabalSyntax getProperty() {
    return this.property;
  }

  public void addFormEntryListener( final IFormEntryListener listener ) {
    this.listeners.add( listener );
  }

  public void removeFormEntryListener( final IFormEntryListener listener ) {
    this.listeners.remove( listener );
  }

  protected void notifyFocusGained() {
    for( IFormEntryListener listener: listeners ) {
      listener.focusGained( this );
    }
  }

  protected void notifyTextDirty() {
    for( IFormEntryListener listener: listeners ) {
      listener.textDirty( this );
    }
  }

  protected void notifyTextValueChanged() {
    for( IFormEntryListener listener: listeners ) {
      listener.textValueChanged( this );
    }
  }

  protected void notifySelectionChanged() {
    for( IFormEntryListener listener: listeners ) {
      listener.selectionChanged( this );
    }
  }
}
