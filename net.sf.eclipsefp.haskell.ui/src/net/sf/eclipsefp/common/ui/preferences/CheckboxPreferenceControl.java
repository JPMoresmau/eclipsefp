package net.sf.eclipsefp.common.ui.preferences;


import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

public class CheckboxPreferenceControl extends PreferenceControl<Boolean> {

	private final Button checkbox;

	public CheckboxPreferenceControl(final Composite parent, final String text, final String name) {
		super(name);

		checkbox = new Button(parent, SWT.CHECK);
		checkbox.setText(text);

		checkbox.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				fireValueChanged();
			}
		});
	}

	@Override
	protected Boolean doLoad(final IPreferenceStore prefStore) {
		return new Boolean(prefStore.getBoolean(getName()));
	}

	@Override
	protected Boolean doLoadDefault(final IPreferenceStore prefStore) {
		return new Boolean(prefStore.getDefaultBoolean(getName()));
	}

	@Override
	protected void doStore(final IPreferenceStore prefStore, final Boolean value) {
		prefStore.setValue(getName(), value.booleanValue());
	}

	@Override
	public Control getControl() {
		return checkbox;
	}

	@Override
	public boolean isEnabled() {
		return checkbox.isEnabled();
	}

	@Override
	public void setEnabled(final boolean enabled) {
		checkbox.setEnabled(enabled);
	}

	@Override
	public Boolean getValue() {
		return new Boolean(checkbox.getSelection());
	}

	@Override
	public void setValue(final Boolean value) {
		checkbox.setSelection(value.booleanValue());
		fireValueChanged();
	}

}
