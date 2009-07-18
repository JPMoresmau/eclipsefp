package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

/**
 * A field editor that consists of a push button.
 * Unlike other field editors, it does not actually store a value,
 * but it can be used to modify values in another field. 
 * 
 * @author Thomas ten Cate
 */
public class ButtonFieldEditor extends FieldEditor {
	
	private String buttonText;
	
	private SelectionListener buttonListener;
	
	private Button button;

	public ButtonFieldEditor(String labelText, String buttonText, SelectionListener buttonListener, Composite parent) {
		this.buttonText = buttonText;
		this.buttonListener = buttonListener;
		setLabelText(labelText);
		createControl(parent);
	}
	
	@Override
	public int getNumberOfControls() {
		return 2;
	}

	@Override
	protected void doFillIntoGrid(Composite parent, int numColumns) {
		Label label = getLabelControl(parent);
		GridData layoutData = new GridData();
		layoutData.horizontalSpan = numColumns - 1;
		layoutData.grabExcessHorizontalSpace = true;
		label.setLayoutData(layoutData);
		
        Button button = getButtonControl(parent);
        setButtonLayoutData(button);
	}
	
	@Override
	protected void adjustForNumColumns(int numColumns) {
        // keep the button right-aligned
		((GridData)getLabelControl().getLayoutData()).horizontalSpan = numColumns - 1;
	}

    /**
     * Get the button control. Create it in parent if required.
     */
	protected Button getButtonControl(Composite parent) {
        if (button == null) {
            button = new Button(parent, SWT.PUSH);
            if (buttonText == null) {
				buttonText = JFaceResources.getString("openChange"); //$NON-NLS-1$
			}
            button.setText(buttonText);
            button.setFont(parent.getFont());
            button.addSelectionListener(buttonListener);
            button.addDisposeListener(new DisposeListener() {
                public void widgetDisposed(DisposeEvent event) {
                    button = null;
                }
            });
        } else {
            checkParent(button, parent);
        }
        return button;
    }
	
	@Override
	public void setEnabled(boolean enabled, Composite parent) {
		super.setEnabled(enabled, parent);
		getButtonControl(parent).setEnabled(enabled);
	}

	@Override
	protected void doLoad() {
		// does nothing
	}

	@Override
	protected void doLoadDefault() {
		// does nothing
	}

	@Override
	protected void doStore() {
		// does nothing
	}

}
