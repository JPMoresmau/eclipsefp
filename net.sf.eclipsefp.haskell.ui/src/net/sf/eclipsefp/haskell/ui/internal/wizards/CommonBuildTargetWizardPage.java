package net.sf.eclipsefp.haskell.ui.internal.wizards;

import net.sf.eclipsefp.haskell.ui.dialog.Validator;
import net.sf.eclipsefp.haskell.ui.dialog.ValidatorManager;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.SelectionButtonDialogField;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.StringDialogField;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;

/**
 * A wizard page that asks the user for the name and type of build target
 * to be created. The type may either be an executable or a library.
 */
public class CommonBuildTargetWizardPage extends
BuildTargetWizardPage {

  private final StringDialogField nameField;
  private final SelectionButtonDialogField executableField, libraryField;

  protected CommonBuildTargetWizardPage( ) {
    super( CommonBuildTargetWizardPage.class.getName() );
    setTitle( UITexts.buildTargetWizardPage_title );
    setDescription( UITexts.buildTargetWizardPage_description );

    nameField = new StringDialogField();
    nameField.setLabelText( UITexts.buildTarget_name_text );

    executableField = new SelectionButtonDialogField( SWT.RADIO );
    executableField.setLabelText( UITexts.buildTargetType_executable_text );
    executableField.setSelection( true );

    libraryField = new SelectionButtonDialogField( SWT.RADIO );
    libraryField.setLabelText( UITexts.buildTargetType_library_text );
  }

  @Override
  protected Control doCreateControl( final Composite parent ) {
    final int numColumns = 2;

    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    GridLayout layout = new GridLayout( numColumns, false );
    layout.verticalSpacing *= 2;
    composite.setLayout( layout );

    Group typeGroup = createTypeGroup( composite );
    GridData layoutData = new GridData( SWT.FILL, SWT.TOP, true, false );
    layoutData.horizontalSpan = numColumns;
    typeGroup.setLayoutData( layoutData );

    nameField.doFillIntoGrid( composite, numColumns );

    return composite;
  }

  @Override
  protected void createValidators( final ValidatorManager manager ) {
    NameValidator nameValidator = new NameValidator();
    TypeValidator typeValidator = new TypeValidator();
    nameField.setDialogFieldListener( nameValidator );
    executableField.setDialogFieldListener( typeValidator );
    libraryField.setDialogFieldListener( typeValidator );
    manager.addValidator( nameValidator );
    manager.addValidator( typeValidator );
  }

  private Group createTypeGroup( final Composite composite ) {
    final int numColumns = 1;

    Group group = new Group( composite, SWT.SHADOW_ETCHED_IN );
    group.setText( UITexts.buildTargetType_text );
    group.setLayout( new GridLayout( numColumns, false ) );

    executableField.doFillIntoGrid( group, numColumns );
    libraryField.doFillIntoGrid( group, numColumns );

    return group;
  }

  public boolean isExecutable() {
    return executableField.isSelected();
  }

  public boolean isLibrary() {
    return libraryField.isSelected();
  }

  public String getTargetName() {
    return nameField.getText();
  }

  private class NameValidator extends Validator {
    @Override
    protected void doUpdate() {
      String name = nameField.getText();
      if (name.length() == 0) {
        setIncomplete( "Please enter a name for the target binary", false );
      } else if (!new Path("").isValidSegment( name )) {
        setIncomplete( "The given binary name is not valid", true );
      }
    }
  }

  private class TypeValidator extends Validator {
    @Override
    protected void doUpdate() {
      if (!(executableField.isSelected() ^ libraryField.isSelected())) {
        setIncomplete( "Please select the type of the build target", false );
      }
    }
  }

}
