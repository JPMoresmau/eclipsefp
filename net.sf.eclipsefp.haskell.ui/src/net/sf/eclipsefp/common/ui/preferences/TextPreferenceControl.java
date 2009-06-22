package net.sf.eclipsefp.common.ui.preferences;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * A preference control based on a text field.
 *
 * @author Thomas ten Cate
 */
public abstract class TextPreferenceControl<T> extends PreferenceControl<T> {

	protected final Label label;
	protected final Text text;
	private final Composite composite;

	public TextPreferenceControl(final Composite parent, final String labelText, final String name) {
		super(name);

		composite = new Composite(parent, SWT.NONE);
		GridLayout gl = new GridLayout();
		gl.numColumns = 2;
		gl.marginWidth = gl.marginHeight = 0;
		composite.setLayout(gl);

		label = new Label(composite, SWT.NONE);
		label.setText(labelText);
		GridData gd = new GridData();
		gd.grabExcessHorizontalSpace = true;
		label.setLayoutData(gd);

		text = new Text(composite, getTextStyle());
		this.text.addModifyListener(new ModifyListener(){
			public void modifyText(final ModifyEvent e) {
				fireValueChanged();
			}
		});
	}

	/**
	 * Returns the style in which the text label should be created.
	 * Called from the {@link #TextPreferenceControl} constructor.
	 * Subclasses may override.
	 */
	protected int getTextStyle() {
		return SWT.BORDER | SWT.SINGLE;
	}

	@Override
	public Control getControl() {
		return composite;
	}

	@Override
	public boolean isEnabled() {
		return label.isEnabled();
	}

	@Override
	public void setEnabled(final boolean enabled) {
		label.setEnabled(enabled);
		text.setEnabled(enabled);
	}

}