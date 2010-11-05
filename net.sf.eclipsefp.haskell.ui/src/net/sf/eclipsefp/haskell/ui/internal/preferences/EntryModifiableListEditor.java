package net.sf.eclipsefp.haskell.ui.internal.preferences;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.preference.ListEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;

/**
* This class extends {@link ListEditor} to enable editing of entries.
*
* @author Sandip V. Chitale
* http://sandipchitale.blogspot.com/2008/09/enhanced-listeditor-implementation.html
*/
public abstract class EntryModifiableListEditor extends ListEditor {

 public EntryModifiableListEditor(final String name, final String labelText, final Composite parent) {
     super(name, labelText, parent);
 }

 /**
  * The subclasses must override this to return the modified entry.
  *
  * @param original the new entry
  * @return the modified entry. Return null to prevent modification.
  */
 protected abstract String getModifiedEntry(String original);

 private Button editButton;
 private List commandListControl;

 @Override
 public Composite getButtonBoxControl(final Composite parent) {
     Composite buttonBoxControl = super.getButtonBoxControl(parent);
     if (editButton == null) {
         editButton = createPushButton(buttonBoxControl, "Edit..."); // TODO I18N
         editButton.setEnabled(false);
         editButton.addSelectionListener(new SelectionAdapter() {
             @Override
             public void widgetSelected(final SelectionEvent e) {
                 if (commandListControl.getSelectionCount() == 1) {
                     String modified = getModifiedEntry(commandListControl.getSelection()[0]);
                     if (modified != null) {
                         int selectedIndex = commandListControl.getSelectionIndex();
                         commandListControl.remove(selectedIndex);
                         commandListControl.add(modified, selectedIndex);
                     }
                 }
             }
         });
         buttonBoxControl.addDisposeListener(new DisposeListener() {
             public void widgetDisposed(final DisposeEvent event) {
                 editButton = null;
             }
         });
     }
     return buttonBoxControl;
 }

 /**
  * Helper method to create a push button.
  *
  * @param parent the parent control
  * @param key the resource name used to supply the button's label text
  * @return Button
  */
 private Button createPushButton(final Composite parent, final String key) {
     Button button = new Button(parent, SWT.PUSH);
     button.setText(key);
     button.setFont(parent.getFont());
     GridData data = new GridData(GridData.FILL_HORIZONTAL);
     int widthHint = convertHorizontalDLUsToPixels(button,
             IDialogConstants.BUTTON_WIDTH);
     data.widthHint = Math.max(widthHint, button.computeSize(SWT.DEFAULT,
             SWT.DEFAULT, true).x);
     button.setLayoutData(data);
     return button;
 }

 @Override
 public List getListControl(final Composite parent) {
     List listControl = super.getListControl(parent);
     if (commandListControl == null) {
         commandListControl = listControl;
         commandListControl.addSelectionListener(new SelectionAdapter() {
             @Override
             public void widgetSelected(final SelectionEvent e) {
                 editButton.setEnabled(commandListControl.getSelectionCount() == 1);
             }
         });
     }
     return listControl;
 }

}
