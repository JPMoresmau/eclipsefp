// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.preferences.hsimpls;


import net.sf.eclipsefp.haskell.core.internal.hsimpl.HsImplementation;
import net.sf.eclipsefp.haskell.core.internal.hsimpl.HsImplementationType;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.DefaultStatus;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.StatusDialog;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;


/** <p>Dialog for the user to add another Haskell implementation.</p>
  *
  * @author Leif Frenzel
  */
public class HsImplementationDialog extends StatusDialog {

  private static final String DIALOG_SETTINGS_ID
    = HsImplementationDialog.class.getName();
	private static final String KEY_DIALOG_HEIGHT = "DIALOG_HEIGHT"; //$NON-NLS-1$
  private static final String KEY_DIALOG_WIDTH = "DIALOG_WIDTH"; //$NON-NLS-1$

	private Combo cmbImplementationType;
	private Text txtName;
	private Text txtBinFolder;
  private final ImplementationsBlock implementationsBlock;

  private final HsImplementation currentImpl;
  private Label lblVersion;
  private Label lblLibDir;

	HsImplementationDialog( final Shell shell,
	                        final ImplementationsBlock implementationsBlock,
	                        final HsImplementation impl ) {
    super( shell );
    setShellStyle( getShellStyle() | SWT.RESIZE );

		this.implementationsBlock = implementationsBlock;
		if( impl == null ) {
		  currentImpl = new HsImplementation();
		} else {
      currentImpl = impl;
		}
		currentImpl.setType( HsImplementationType.GHC );
	}


	// interface methods of StatusDialog
	////////////////////////////////////

  @Override
  protected Control createDialogArea( final Composite parent ) {
    Composite composite = ( Composite )super.createDialogArea( parent );
    ( ( GridLayout )composite.getLayout() ).numColumns = 3;

    createLabel( composite, UITexts.hsImplementationDialog_type, 1 );
	  String[] types = new String[] { HsImplementationType.GHC.toString() };
    cmbImplementationType = createCombo( composite, types );
    createLabel( composite, UITexts.hsImplementationDialog_name, 1 );
    txtName = createSingleText( composite, 2 );
    createLabel( composite, UITexts.hsImplementationDialog_binDir, 1 );
    txtBinFolder = createSingleText( composite, 1 );
    createBrowseButton( composite );
    createLabel( composite, UITexts.hsImplementationDialog_version, 1 );
    lblVersion = createLabel( composite, "", 2 ); //$NON-NLS-1$
    createLabel( composite, UITexts.hsImplementationDialog_libDir, 1 );
    lblLibDir = createLabel( composite, "", 2 ); //$NON-NLS-1$

    initializeFields();
    txtName.addModifyListener( new ModifyListener() {
      public void modifyText( final ModifyEvent evt ) {
        currentImpl.setName( txtName.getText() );
        validate();
        updateFields();
      }
    } );
    txtBinFolder.addModifyListener( new ModifyListener() {
      public void modifyText( final ModifyEvent evt ) {
        currentImpl.setBinDir( txtBinFolder.getText() );
        validate();
        updateFields();
      }
    } );
    applyDialogFont( composite );
    return composite;
  }

	@Override
  protected void updateButtonsEnableState( final IStatus status ) {
	  Button ok = getButton( IDialogConstants.OK_ID );
	  if( ok != null && !ok.isDisposed() ) {
	    ok.setEnabled( status.getSeverity() == IStatus.OK );
	  }
	}

  @Override
  protected IDialogSettings getDialogBoundsSettings() {
    IDialogSettings settings = HaskellUIPlugin.getDefault().getDialogSettings();
    IDialogSettings section = settings.getSection( DIALOG_SETTINGS_ID );
    if( section == null ) {
      section = settings.addNewSection( DIALOG_SETTINGS_ID );
    }
    return section;
  }

  @Override
  protected Point getInitialSize() {
    IDialogSettings settings = getDialogBoundsSettings();
    if( settings != null ) {
      try {
        int width = settings.getInt( KEY_DIALOG_WIDTH );
        int height = settings.getInt( KEY_DIALOG_HEIGHT );
        if( width > 0 & height > 0 ) {
          return new Point( width, height );
        }
      } catch( NumberFormatException nfe ) {
        return new Point( 500, 570 );
      }
    }
    return new Point( 500, 570 );
  }

  @Override
  public void create() {
    super.create();
    txtName.setFocus();
  }

  @Override
  protected void okPressed() {
    implementationsBlock.add( currentImpl );
    super.okPressed();
  }


  // helping functions
  ////////////////////

  private void validate() {
    if( implementationsBlock.isDuplicateName( txtName.getText() ) ) {
      String msg = UITexts.hsImplementationDialog_duplicate;
      DefaultStatus status = new DefaultStatus();
      status.setError( NLS.bind( msg, new String[] { txtName.getText() } ) );
      updateStatus( status );
    } else {
      IStatus[] statuss = currentImpl.validate();
      IStatus max = null;
      for( int i = 0; i < statuss.length; i++ ) {
        IStatus curr = statuss[ i ];
        if( curr.matches( IStatus.ERROR ) ) {
          updateStatus( curr );
          return;
        }
        if( max == null || curr.getSeverity() > max.getSeverity() ) {
          max = curr;
        }
      }
      updateStatus( max );
    }
  }

  private void createBrowseButton( final Composite composite ) {
    String text = UITexts.hsImplementationDialog_btnBrowse;
    Button browse = SWTUtil.createPushButton( composite, text );
    browse.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        DirectoryDialog dialog = new DirectoryDialog( getShell() );
        dialog.setFilterPath( txtBinFolder.getText() );
        dialog.setMessage( UITexts.hsImplementationDialog_dlgBrowse );
        String newPath = dialog.open();
        if( newPath != null ) {
          txtBinFolder.setText( newPath );
        }
      }
    } );
  }

  private Combo createCombo( final Composite parent, final String[] items ) {
    Combo result = new Combo( parent, SWT.READ_ONLY );
    result.setFont( parent.getFont() );
    GridData gd = new GridData( GridData.FILL_HORIZONTAL );
    gd.horizontalSpan = 2;
    result.setLayoutData( gd );
    result.setItems( items );
    result.select( 0 );
    return result;
  }

  private Label createLabel( final Composite parent,
                             final String text,
                             final int hspan ) {
    Label result = new Label( parent, SWT.NONE );
    result.setFont( parent.getFont() );
    result.setText( text );
    GridData gd = new GridData( GridData.FILL_HORIZONTAL );
    gd.horizontalSpan = hspan;
    result.setLayoutData( gd );
    return result;
  }

  public static Text createSingleText( final Composite parent,
                                       final int hspan ) {
    Text result = new Text( parent, SWT.SINGLE | SWT.BORDER );
    result.setFont( parent.getFont() );
    GridData gd = new GridData( GridData.FILL_HORIZONTAL );
    gd.horizontalSpan = hspan;
    result.setLayoutData( gd );
    return result;
  }

  private void initializeFields() {
    txtName.setText( "" ); //$NON-NLS-1$
    txtBinFolder.setText( "" ); //$NON-NLS-1$
    cmbImplementationType.setEnabled( false );
    validate();
    updateFields();
  }

  private void updateFields() {
    String vs = currentImpl.getVersion();
    if( vs == null ) {
      vs = ""; //$NON-NLS-1$
    }
    lblVersion.setText( vs.trim() );
    String ld = currentImpl.getLibDir();
    if( ld == null ) {
      ld = ""; //$NON-NLS-1$
    }
    lblLibDir.setText( ld.trim() );
  }
}
