package net.sf.eclipsefp.common.ui.preferences;

/**
 * A listener that listens for changes in the value of a preference control.
 *
 * @author Thomas ten Cate
 */
public interface IValueChangedListener<T> {

	public void valueChanged(PreferenceControl<T> control, T value);

}
