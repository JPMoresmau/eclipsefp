// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.wizards;

import java.io.File;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/** <p>The UI portion for selecting a cabalized archive of a package from
  * the file system.</p>
  *
  * @author Leif Frenzel
  */
public class CabalPackageSelectionBlock {

	public interface IErrorMessageReporter {
    public void reportError( String errorMessage );
  }

	private final IErrorMessageReporter errorReporter;

	private Label lblLocation;
	private Text txtLocation;
	private Button btnBrowse;


	CabalPackageSelectionBlock(
	    final IErrorMessageReporter reporter,
      final Composite composite ) {
    errorReporter = reporter;
    createContents( composite );
  }

	String getSelectedArchiveName() {
	  return txtLocation.getText();
	}


	// helping methods
	//////////////////

	private void handleLocationBrowseButtonPressed() {
    FileDialog dialog = new FileDialog( txtLocation.getShell() );
    dialog.setText( "Select a cabalized archive" );
    dialog.setFilterExtensions( new String[] { "tar.gz" } );
    String result = dialog.open();
    if( result != null && result.trim().length() > 0 ) {
      txtLocation.setText( result );
    }
	}

  private String checkValidLocation() {
    String errorMsg = null;
    String name = txtLocation.getText();
    if( name == null || name.trim().length() == 0 ) {
      errorMsg = "Please select a cabalized archive";
    }

    File file = new File( name );
    if( !isValidArchiveFile( file ) ) {
      errorMsg = "Not a valid cabal package: " + name;
    }
    return errorMsg;
  }

  private boolean isValidArchiveFile( final File file ) {
    return    file.exists()
           && !file.isDirectory()
           && file.getName().endsWith( ".tar.gz" );
  }

  private void createContents( final Composite parent ) {
    Composite comp = new Composite( parent, SWT.NONE );
    GridLayout layout = new GridLayout();
    layout.numColumns = 4;
    comp.setLayout( layout );
    comp.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    createUserEntryArea( comp );
  }

  private void createUserEntryArea( final Composite composite ) {
    lblLocation = new Label( composite, SWT.NONE );
    lblLocation.setText( "Location" );

    txtLocation = new Text( composite, SWT.BORDER );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    data.widthHint = 250;
    data.horizontalSpan = 2;
    txtLocation.setLayoutData( data );
    txtLocation.setText( "" );
    txtLocation.addModifyListener( new ModifyListener() {
      public void modifyText( final ModifyEvent e ) {
        errorReporter.reportError( checkValidLocation() );
      }
    } );

    btnBrowse = new Button( composite, SWT.PUSH );
    btnBrowse.setText( "Browse" );
    btnBrowse.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent event ) {
        handleLocationBrowseButtonPressed();
      }
    } );
  }
}
