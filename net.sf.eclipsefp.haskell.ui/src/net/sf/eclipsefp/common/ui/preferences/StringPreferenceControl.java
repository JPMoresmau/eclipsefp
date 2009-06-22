package net.sf.eclipsefp.common.ui.preferences;

import net.sf.eclipsefp.common.ui.util.DialogUtil;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

public class StringPreferenceControl extends TextPreferenceControl<String> {

	public StringPreferenceControl(final Composite parent, final String labelText, final String name, final int maxLength) {
		super(parent, labelText, name);

		if (maxLength > 0) {
			text.setTextLimit(maxLength);
			GridData gd = new GridData();
			gd.widthHint = DialogUtil.convertWidthInCharsToPixels(text, maxLength + 1);
			text.setLayoutData(gd);
		}
	}

	@Override
	public String getValue() {
		return text.getText();
	}

	@Override
	public void setValue(final String value) {
		text.setText(value);
		fireValueChanged();
	}

	@Override
	public String doLoad(final IPreferenceStore prefStore) {
		return prefStore.getString(getName());
	}

	@Override
	public String doLoadDefault(final IPreferenceStore prefStore) {
		return prefStore.getDefaultString(getName());
	}

	@Override
	public void doStore(final IPreferenceStore prefStore, final String value) {
		prefStore.setValue(getName(), value);
	}

}
