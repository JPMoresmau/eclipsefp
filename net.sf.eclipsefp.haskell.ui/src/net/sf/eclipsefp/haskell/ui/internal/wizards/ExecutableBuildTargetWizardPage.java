package net.sf.eclipsefp.haskell.ui.internal.wizards;

import net.sf.eclipsefp.haskell.ui.dialog.Validator;
import net.sf.eclipsefp.haskell.ui.dialog.ValidatorManager;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.StringDialogField;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;


public class ExecutableBuildTargetWizardPage extends BuildTargetWizardPage {

  StringDialogField mainField;

  public ExecutableBuildTargetWizardPage() {
    super( ExecutableBuildTargetWizardPage.class.getName() );
    setTitle( UITexts.executableBuildTargetWizardPage_title );
    setDescription( UITexts.executableBuildTargetWizardPage_description );
    mainField = new StringDialogField();
    mainField.setLabelText( UITexts.executableBuildTarget_main_text );
    mainField.setText( "Main.main" );
  }

  public String getMain() {
    return mainField.getText();
  }

  @Override
  protected void createValidators( final ValidatorManager manager ) {
    MainValidator mainValidator = new MainValidator();
    mainField.setDialogFieldListener( mainValidator );
    manager.addValidator( mainValidator );
  }

  @Override
  protected Control doCreateControl( final Composite parent ) {
    final int numColumns = 2;

    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayoutData( new GridData( SWT.FILL, SWT.TOP, true, false ) );
    composite.setLayout( new GridLayout(numColumns, false) );

    mainField.doFillIntoGrid( composite, numColumns );

    return composite;
  }

  private class MainValidator extends Validator {
    @Override
    protected void doUpdate() {
      if (mainField.getText().length() == 0) {
        setIncomplete( "Please provide the fully qualified name of the main function, for example \"Main.main\".", false );
      }
    }
  }

}
