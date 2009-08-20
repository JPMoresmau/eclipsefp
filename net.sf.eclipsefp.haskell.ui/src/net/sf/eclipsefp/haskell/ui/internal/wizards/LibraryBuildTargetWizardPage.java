package net.sf.eclipsefp.haskell.ui.internal.wizards;

import net.sf.eclipsefp.haskell.ui.dialog.ValidatorManager;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

/**
 * A wizard page that allows the user to add modules to a library.
 *
 * TODO TtC implement this page
 *
 * @author Thomas ten Cate
 */
public class LibraryBuildTargetWizardPage extends BuildTargetWizardPage {

  public LibraryBuildTargetWizardPage() {
    super( LibraryBuildTargetWizardPage.class.getName() );
    setTitle( UITexts.libraryBuildTargetWizardPage_title );
    setDescription( UITexts.libraryBuildTargetWizardPage_description );
  }

  @Override
  protected void createValidators( final ValidatorManager manager ) {
    // nothing to do
  }

  @Override
  protected Control doCreateControl( final Composite parent ) {
    final int numColumns = 1;

    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout(numColumns, false) );

    Label unimplemented = new Label(composite, SWT.LEFT);
    unimplemented.setText( "Sorry, this feature is not yet implemented." );

    return composite;
  }

}
