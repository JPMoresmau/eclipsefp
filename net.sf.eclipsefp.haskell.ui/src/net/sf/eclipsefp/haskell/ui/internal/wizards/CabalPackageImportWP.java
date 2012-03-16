// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.wizards;

import java.io.File;
import java.util.Observable;
import net.sf.eclipsefp.haskell.ui.dialog.Validator;
import net.sf.eclipsefp.haskell.ui.dialog.ValidatorManager;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.DialogField;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.IDialogFieldListener;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.IStringButtonAdapter;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.LayoutUtil;
import net.sf.eclipsefp.haskell.ui.dialog.dialogfields.StringButtonDialogField;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;

/**
 * <p>
 * wizard page for importing a cabalized package. This is basically the page for
 * creating a new project, but with the additional selection of the package
 * archive from the file system..
 * </p>
 *
 * @author Leif Frenzel
 */
class CabalPackageImportWP extends NewProjectWizardPage {

  public class PackageGroup extends Observable implements IStringButtonAdapter,
      IDialogFieldListener {

    private final StringButtonDialogField fLocation;

    public PackageGroup() {
      fLocation = new StringButtonDialogField( this );
      fLocation.setDialogFieldListener( this );
      fLocation
          .setLabelText( UITexts.cabalPackageImportWP_PackageGroup_locationLabel_desc );
      fLocation
          .setButtonLabel( UITexts.cabalPackageImportWP_LocationGroup_browseButton_desc );
    }

    String getSelectedArchiveName() {
      return fLocation.getText();
    }


    // helping methods
    // ////////////////

    public Control createControl( final Composite parent ) {
      final int numColumns = 3;

      final Composite composite = new Composite( parent, SWT.NONE );
      composite.setLayout( initGridLayout( new GridLayout( 3, false ), false ) );

      fLocation.doFillIntoGrid( composite, numColumns );
      LayoutUtil.setHorizontalGrabbing( fLocation.getTextControl( null ) );

      return composite;
    }

    @Override
    public void changeControlPressed( final DialogField field ) {
      FileDialog dialog = new FileDialog( getShell() );
      dialog.setText( "Select a cabalized archive" );
      dialog.setFilterExtensions( new String[] { "*.tar.gz" } );
      String result = dialog.open();
      if( result != null && result.trim().length() > 0 ) {
        fLocation.setText( result );
      }
    }

    @Override
    public void dialogFieldChanged( final DialogField field ) {
      setChanged();
      notifyObservers();
    }

  }

  private final class PageValidator extends Validator {

    public PageValidator( final ValidatorManager manager ) {
      super( manager );
    }

    @Override
    protected void doUpdate() {
      String name = fPackageGroup.getSelectedArchiveName();
      if( name == null || name.trim().length() == 0 ) {
        setIncomplete( "Please select a cabalized archive", false );
        return;
      }

      File file = new File( name );
      if( !isValidArchiveFile( file ) ) {
        setIncomplete( "Not a valid cabal package: " + name, true );
        return;
      }
    }

    private boolean isValidArchiveFile( final File file ) {
      return file.exists() && !file.isDirectory()
          && file.getName().endsWith( ".tar.gz" );
    }

  }

  private static final String PAGE_NAME = "CabalPackageImportWP"; //$NON-NLS-1$

  private final PackageGroup fPackageGroup;
  private PageValidator fPageValidator;

  CabalPackageImportWP() {
    super( PAGE_NAME );
    fPackageGroup = new PackageGroup();

    fPackageGroup.addObserver( fPageValidator );
  }

  @Override
  protected void createValidators( final ValidatorManager manager ) {
    fPageValidator = new PageValidator( manager );
    super.createValidators( manager );
  }

  String getArchiveLocation() {
    return fPackageGroup.getSelectedArchiveName();
  }

  // ////////////////////////////////////
  // methods from NewProjectWizardPage

  @Override
  protected void createControls( final Composite parent ) {
    createNameControl( parent );
    createPackageControl( parent );
    createLocationControl( parent );
  }

  private Control createPackageControl( final Composite parent ) {
    Control packageControl = fPackageGroup.createControl( parent );
    packageControl.setLayoutData( horizontalFillGridData() );
    return packageControl;
  }

}