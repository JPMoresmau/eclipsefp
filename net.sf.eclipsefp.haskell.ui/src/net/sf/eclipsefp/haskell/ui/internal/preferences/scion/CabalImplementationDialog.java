// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import net.sf.eclipsefp.haskell.core.cabal.CabalImplementation;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.StatusDialog;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;


/** <p>TODO</p>
 *
 * @author Leif Frenzel
 */
public class CabalImplementationDialog extends StatusDialog {
  private static final String DIALOG_SETTINGS_ID = CabalImplementationDialog.class.getName();
  private static final String KEY_DIALOG_HEIGHT = "DIALOG_HEIGHT"; //$NON-NLS-1$
  private static final String KEY_DIALOG_WIDTH = "DIALOG_WIDTH"; //$NON-NLS-1$
  /** The Cabal implementation block (preference page UI) */
  private final CabalImplsBlock theBlock;

  /** The current Cabal implementation */
  private final CabalImplementation currentImpl;

  /** The base name of the cabal executable */
  private Text txtName;
  /** Path component of the cabal executable */
  private Text txtBinDir;
  /** Cabal library version label widget */
  private Label lblCabalLibraryVersion;
  /** Cabal version label widget */
  private Label lblCabalInstallVersion;

  /** The dialog constructor
   * @param shell The shell that controls this dialog
   * @param implementationsBlock The Cabal implementations perference page
   * @param impl The Cabal implementation
   */
  CabalImplementationDialog( final Shell shell,
      final CabalImplsBlock cabalImplsBlock, final CabalImplementation impl ) {
    super( shell );
    theBlock = cabalImplsBlock;
    currentImpl = new CabalImplementation();
    if( impl != null ) {
      // Fill in details as necessary.
    }
  }

  CabalImplementation getResult() {
    return currentImpl;
  }
  @Override
  protected Control createDialogArea( final Composite parent ) {
    Composite composite = null;

    try {
      composite = ( Composite ) super.createDialogArea( parent );
      GridLayout glayout = ( GridLayout ) composite.getLayout();
      glayout.numColumns = 3;
    } catch (ClassCastException e) {
      // Should never happen... :-)
    }

    SWTUtil.createLabel( composite, UITexts.cabalImplsDialog_name, 1 );
    txtName = SWTUtil.createSingleText( composite, 2 );
    SWTUtil.createLabel( composite, UITexts.hsImplementationDialog_binDir, 1 );
    txtBinDir = SWTUtil.createSingleText( composite, 1 );
    createBrowseButton( composite );
    SWTUtil.createLabel( composite, UITexts.cabalImplsDialog_libVersion, 1 );
    lblCabalLibraryVersion = SWTUtil.createLabel( composite, "", 2 ); //$NON-NLS-1$
    SWTUtil.createLabel( composite, UITexts.cabalImplsDialog_installVersion, 1 );
    lblCabalInstallVersion = SWTUtil.createLabel( composite, "", 2 ); //$NON-NLS-1$

    /*
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
    */
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

  private void createBrowseButton( final Composite composite ) {
    String text = UITexts.hsImplementationDialog_btnBrowse;
    Button browse = SWTUtil.createPushButton( composite, text );
    browse.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        DirectoryDialog dialog = new DirectoryDialog( getShell() );
        dialog.setFilterPath( txtBinDir.getText() );
        dialog.setMessage( UITexts.hsImplementationDialog_dlgBrowse );
        String newPath = dialog.open();
        if( newPath != null ) {
          txtBinDir.setText( newPath );
        }
      }
    } );
  }
}
