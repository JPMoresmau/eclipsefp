// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.wizards;

import net.sf.eclipsefp.haskell.ui.internal.wizards.CabalPackageSelectionBlock.IErrorMessageReporter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;

/** <p>wizard page for importing a cabalized package. This is basically
  * the page for creating a new project, but with the additional selection
  * of the package archive from the file system..</p>
  *
  * @author Leif Frenzel
  */
class CabalPackageImportWP extends WizardNewProjectCreationPage {

  private CabalPackageSelectionBlock cabalPackageSelectionBlock;

  CabalPackageImportWP( final String name ) {
    super( name );
  }

  String getArchiveLocation() {
    return cabalPackageSelectionBlock.getSelectedArchiveName();
  }


  // interface methods of IWizardPage
  ///////////////////////////////////

  @Override
  public void createControl( final Composite parent ) {
    Composite mainComposite = initMainComposite( parent );
    Composite group = new Composite( mainComposite, SWT.NONE );
    group.setLayout( new GridLayout() );
    group.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    cabalPackageSelectionBlock
      = new CabalPackageSelectionBlock( getErrorReporter(), group );
    // let the super class create the rest of the project creation controls
    super.createControl( mainComposite );
  }


  // helping methods
  //////////////////

  private Composite initMainComposite( final Composite parent ) {
    Composite result = new Composite( parent, SWT.NULL );
    result.setFont( parent.getFont() );
    initializeDialogUnits( parent );
    GridLayout gridLayout = new GridLayout();
    gridLayout.marginTop = 0;
    gridLayout.marginRight = 0;
    gridLayout.marginBottom = 0;
    gridLayout.marginLeft = 0;
    result.setLayout( gridLayout );
    result.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    return result;
  }

  private IErrorMessageReporter getErrorReporter() {
    return new IErrorMessageReporter() {
      public void reportError( final String errorMessage ) {
        setErrorMessage( errorMessage );
        boolean valid = errorMessage == null;
        if( valid ) {
          valid = validatePage();
        }
        setPageComplete( valid );
      }
    };
  }
}