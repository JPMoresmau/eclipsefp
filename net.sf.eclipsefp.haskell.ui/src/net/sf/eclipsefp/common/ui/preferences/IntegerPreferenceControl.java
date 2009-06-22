package net.sf.eclipsefp.common.ui.preferences;

import net.sf.eclipsefp.common.ui.util.DialogUtil;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;

public class IntegerPreferenceControl extends TextPreferenceControl<Integer> {

	public IntegerPreferenceControl(final Composite parent, final String labelText,
			final String name, final int maxLength) {
		super(parent, labelText, name);

		if (maxLength > 0) {
			text.setTextLimit(maxLength);
			GridData gd = new GridData();
			gd.widthHint = DialogUtil.convertWidthInCharsToPixels(text, maxLength + 1);
			text.setLayoutData(gd);
		}
	}

	@Override
	protected int getTextStyle() {
		return super.getTextStyle() | SWT.RIGHT;
	}

	@Override
	protected Integer doLoad(final IPreferenceStore prefStore) {
		return new Integer(prefStore.getInt(getName()));
	}

	@Override
	protected Integer doLoadDefault(final IPreferenceStore prefStore) {
		return new Integer(prefStore.getDefaultInt(getName()));
	}

	@Override
	protected void doStore(final IPreferenceStore prefStore, final Integer value) {
		prefStore.setValue(getName(), value.intValue());
	}

	@Override
	public Integer getValue() {
		return new Integer(text.getText());
	}

	@Override
	public void setValue(final Integer value) {
		text.setText(value.toString());
		fireValueChanged();
	}

}
