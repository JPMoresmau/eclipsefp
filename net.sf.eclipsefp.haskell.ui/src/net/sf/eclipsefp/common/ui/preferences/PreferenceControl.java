package net.sf.eclipsefp.common.ui.preferences;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Control;

/**
 * A control that represents a preference on a preference page.
 *
 * @author Thomas ten Cate
 */
public abstract class PreferenceControl<T> {

	private List<IValueChangedListener<T>> valueChangedListeners = null;
	private final String name;

	protected PreferenceControl(final String name) {
		this.name = name;
	}

	/**
	 * Returns the name of the associated preference.
	 */
	protected String getName() {
		return name;
	}

	/**
	 * Loads the value from the preference store into the control.
	 */
	public void load(final IPreferenceStore prefStore) {
		setValue(doLoad(prefStore));
	}

	/**
	 * Loads the default value from the preference store into the control.
	 */
	public void loadDefault(final IPreferenceStore prefStore) {
		setValue(doLoadDefault(prefStore));
	}

	/**
	 * Saves the value from the control into the preference store.
	 */
	public void store(final IPreferenceStore prefStore) {
		doStore(prefStore, getValue());
	}

	/**
	 * Loads the value from the given preference store, and returns it.
	 * Does not change the actual value in the control.
	 */
	protected abstract T doLoad(IPreferenceStore prefStore);

	/**
	 * Loads the default value from the given preference store, and returns it.
	 * Does not change the actual value in the control.
	 */
	protected abstract T doLoadDefault(IPreferenceStore prefStore);

	/**
	 * Saves the given value to the given preference store.
	 * Does not read the actual value from the control.
	 */
	protected abstract void doStore(IPreferenceStore prefStore, T value);

	/**
	 * Returns the control implementing this preference.
	 */
	public abstract Control getControl();

	/**
	 * Returns whether the control is currently enabled.
	 */
	public abstract boolean isEnabled();

	/**
	 * Sets the enablement of the control.
	 */
	public abstract void setEnabled(boolean enabled);

	/**
	 * Returns the current value of this control.
	 */
	public abstract T getValue();

	/**
	 * Sets a new value for the control.
	 * Subclasses should probably cal {@link #fireValueChanged()} from their implementations.
	 */
	public abstract void setValue(T value);

	/**
	 * Adds a new listener that will be notified after the value of this
	 * control has changed.
	 * @param init If true, an event will be fired immediately to the new listener.
	 */
	public void addValueChangedListener(final IValueChangedListener<T> listener, final boolean init) {
		if (valueChangedListeners == null) {
			valueChangedListeners = new ArrayList<IValueChangedListener<T>>();
		}
		valueChangedListeners.add(listener);
		if (init) {
			listener.valueChanged(this, getValue());
		}
	}

	/**
	 * Removes a value-changed listener.
	 */
	public void removeValueChangedListener(final IValueChangedListener listener) {
		if (valueChangedListeners != null) {
			valueChangedListeners.remove(listener);
		}
	}

	/**
	 * Fires a value-changed event.
	 */
	protected void fireValueChanged() {
		if (valueChangedListeners != null) {
			for (IValueChangedListener<T> listener : valueChangedListeners) {
				listener.valueChanged(this, getValue());
			}
		}
	}

}
