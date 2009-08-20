package net.sf.eclipsefp.haskell.ui.internal.dialogs;


import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.StringDialogField;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * A dialog that allows editing of a build target.
 * Since the only thing all build targets have in common is a file name,
 * this provides only a field for the name. Subclasses can add more fields.
 *
 * @author Thomas ten Cate
 */
public abstract class EditBuildTargetDialog extends Dialog {

  private String title;
  protected StringDialogField nameField;

  public EditBuildTargetDialog( final Shell parentShell ) {
    super( parentShell );
    setTitle( UITexts.editExecutableBuildTargetDialog_title );
  }

  public EditBuildTargetDialog( final IShellProvider parentShell ) {
    this( parentShell.getShell() );
  }

  /**
   * Sets the title of this dialog.
   * Only has an effect before it is opened.
   * This may be called from the subclass constructor.
   */
  protected void setTitle( final String title ) {
    this.title = title;
  }

  /**
   * Returns a {@link Composite} with a two-column grid layout,
   * containing a text field for the build target's name.
   * Subclasses may override and add more fields to the returned composite.
   */
  @Override
  protected Control createDialogArea( final Composite parent ) {
    Composite composite = (Composite) super.createDialogArea(parent);
    GridLayout layout = (GridLayout) composite.getLayout();
    layout.numColumns = 2;

    nameField.doFillIntoGrid( composite, 2 );

    return composite;
  }

  @Override
  protected void configureShell( final Shell newShell ) {
    super.configureShell( newShell );
    newShell.setText( title );
  }

}