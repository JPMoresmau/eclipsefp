// Copyright (c) 2010 B. Scott Michel (bscottm@ieee.org)
package net.sf.eclipsefp.haskell.ui.internal.preferences.scion;

import net.sf.eclipsefp.haskell.core.cabal.CabalImplementation;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.SWTUtil;
import net.sf.eclipsefp.haskell.util.FileUtil;
import net.sf.eclipsefp.haskell.util.PlatformUtil;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.StatusDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/** The Add/Edit Cabal implementation dialog box.
 *
 * @author B. Scott Michel (bscottm@ieee.org)
 */
public class CabalImplementationDialog extends StatusDialog {
  private static final String DIALOG_SETTINGS_ID = CabalImplementationDialog.class.getName();
  private static final String KEY_DIALOG_HEIGHT = "DIALOG_HEIGHT"; //$NON-NLS-1$
  private static final String KEY_DIALOG_WIDTH = "DIALOG_WIDTH"; //$NON-NLS-1$

  /** The current Cabal implementation */
  private final CabalImplementation currentImpl;

  /** Installed Cabal executable identifier */
  private Text txtIdent;
  /** Path to the cabal executable */
  private Text txtExecutablePath;
  /** Cabal library version label widget */
  private Label lblCabalLibraryVersion;
  /** Cabal version label widget */
  private Label lblCabalInstallVersion;

  /** The dialog constructor
   * @param shell The shell that controls this dialog
   * @param impl The Cabal implementation
   */
  CabalImplementationDialog( final Shell shell, final CabalImplementation impl ) {
    super( shell );
    currentImpl = new CabalImplementation (impl);
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
    txtIdent = SWTUtil.createSingleText( composite, 2 );
    SWTUtil.createLabel( composite, UITexts.cabalImplsDialog_executablePath, 1 );
    txtExecutablePath = SWTUtil.createSingleText( composite, 1 );
    createBrowseButton( composite );
    SWTUtil.createLabel( composite, UITexts.cabalImplsDialog_libVersion, 1 );
    lblCabalLibraryVersion = SWTUtil.createLabel( composite, "", 2 ); //$NON-NLS-1$
    SWTUtil.createLabel( composite, UITexts.cabalImplsDialog_installVersion, 1 );
    lblCabalInstallVersion = SWTUtil.createLabel( composite, "", 2 ); //$NON-NLS-1$

    initializeFields();
    validate();

    txtIdent.addModifyListener( new ModifyListener() {
      @Override
      public void modifyText( final ModifyEvent evt ) {
        currentImpl.setUserIdentifier( txtIdent.getText( ).trim() );
        validate();
      }
    } );
    txtExecutablePath.addModifyListener( new ModifyListener() {
      @Override
      public void modifyText( final ModifyEvent evt ) {
        updateExecutable();
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

  private void createBrowseButton( final Composite composite ) {
    String text = UITexts.cabalImplsDialog_btnBrowse;
    Button browse = SWTUtil.createPushButton( composite, text );
    browse.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        FileDialog dialog = new FileDialog( getShell(), SWT.OPEN );
        dialog.setFilterPath( txtExecutablePath.getText() );
        if (PlatformUtil.runningOnWindows()) {
          dialog.setFilterNames( new String[] { "Executables" } );
          dialog.setFilterExtensions ( new String[] { "*.".concat( PlatformUtil.WINDOWS_EXTENSION_EXE ) } );
        } else {
          dialog.setFilterNames( new String [] { "Files" } );
          dialog.setFilterExtensions( new String [] { "*" } );
        }
        dialog.setText( UITexts.cabalImplsDialog_dlgBrowse );
        String newPath = dialog.open();
        if( newPath != null ) {
          txtExecutablePath.setText( newPath );
          updateExecutable();
        }
      }
    } );
  }

  private void initializeFields () {
    String s = currentImpl.getUserIdentifier();
    if (s == null) {
      s = new String();
    }
    txtIdent.setText( s.trim() );

    IPath p = currentImpl.getCabalExecutableName();
    if (p != null) {
      s = p.toOSString();
    } else {
      s = new String();
    }
    txtExecutablePath.setText( s );

    s = currentImpl.getInstallVersion();
    if (s == null) {
      s = new String();
    }

    lblCabalInstallVersion.setText( s.trim() );

    s = currentImpl.getLibraryVersion();
    if ( s == null ) {
      s = new String();
    }
    lblCabalLibraryVersion.setText( s.trim() );
  }

  private void updateFields () {
    String installVersion = currentImpl.getInstallVersion();
    String libraryVersion = currentImpl.getLibraryVersion();

    if ( installVersion == null ) {
      installVersion = new String();
    }

    lblCabalInstallVersion.setText( installVersion.trim() );

    if ( libraryVersion == null ) {
      libraryVersion = new String();
    }

    lblCabalLibraryVersion.setText( libraryVersion.trim() );
  }

  private void updateExecutable () {
    String exeName = txtExecutablePath.getText();
    if ( exeName.length() > 0 ) {
      IPath exePath = FileUtil.makeExecutableName( new Path(exeName) );
      currentImpl.setCabalExecutableName( exePath );
      validate();
      updateFields();
    }
  }

  private void validate() {
    String userIdent = currentImpl.getUserIdentifier();
    String installVersion = currentImpl.getInstallVersion();
    String libraryVersion = currentImpl.getLibraryVersion();

    if (   userIdent != null
        && userIdent.length() > 0
        && installVersion != null
        && installVersion.length() > 0
        && libraryVersion != null
        && libraryVersion.length() > 0) {
      updateStatus( new Status( IStatus.OK, HaskellUIPlugin.getPluginId(), "" ) );
    } else {
      String msg = null;
      if (   userIdent == null
          || userIdent.length() <= 0) {
        msg = UITexts.cabalImplsDialog_invalidUserIdentifier;
      } else {
        msg = UITexts.cabalImplDialog_invalidCabalExecutable;
      }
      updateStatus( new Status( IStatus.ERROR, HaskellUIPlugin.getPluginId(), msg) );
    }
  }
}
