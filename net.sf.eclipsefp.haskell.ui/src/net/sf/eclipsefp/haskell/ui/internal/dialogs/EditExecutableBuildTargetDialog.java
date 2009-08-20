package net.sf.eclipsefp.haskell.ui.internal.dialogs;

import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.StringDialogField;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * A dialog that allows editing of an executable build target.
 * It has fields to set its name and the name of its main function.
 *
 * @author Thomas ten Cate
 */
public class EditExecutableBuildTargetDialog extends
EditBuildTargetDialog {

  StringDialogField mainField;

  public EditExecutableBuildTargetDialog( final IShellProvider parentShell ) {
    this( parentShell.getShell() );
  }

  public EditExecutableBuildTargetDialog( final Shell parentShell ) {
    super( parentShell );
    setBlockOnOpen( true );
    nameField = new StringDialogField();
    nameField.setLabelText( UITexts.buildTarget_name_text );
    mainField = new StringDialogField();
    mainField.setLabelText( UITexts.executableBuildTarget_main_text );
  }

  @Override
  protected Control createDialogArea( final Composite parent ) {
    Composite composite = (Composite) super.createDialogArea( parent );
    mainField.doFillIntoGrid( composite, 2 );
    return composite;
  }

}
