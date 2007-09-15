// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.dialog.ExecutableDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.haskell.ghccompiler.core.IGhcParameters;
import net.sf.eclipsefp.haskell.ghccompiler.core.Util;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Text;

/** <p>The tab on the Ghc compiler preference page that displays general 
  * information about the ghc compiler installed on the machine (if any).</p>
  * 
  * @author Leif Frenzel
  */
public class GeneralTab extends Tab implements IGhcParameters, 
                                               IGhcPreferenceNames {
  
  public GeneralTab( final IPreferenceStore store ) {
    super( store );
  }

  // interface methods of Tab
  ///////////////////////////
  
  @Override
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );
    
    createExecutableField( composite );
    createExtraOptionsField( composite );
    
    return composite;
  }

  
  // helping methods
  //////////////////
  
  private void createExtraOptionsField( final Composite parent ) {
    Composite wrapper = new Composite( parent, SWT.NONE );
    wrapper.setLayout( new GridLayout( 1, false ) );
    
    Group group = new Group( wrapper, SWT.NONE );
    group.setLayout( new GridLayout() );
    group.setText( "Extra compiler options" );
    group.setLayoutData( new GridData( GridData.FILL_HORIZONTAL) );
    
    final Button cbActive = new Button( group, SWT.CHECK );
    String msg 
      = "Pass these options (please be careful, they are not validated ...):";
    cbActive.setText( msg );
    boolean selected = getPreferenceStore().getBoolean( USE_EXTRA_OPTIONS );
    cbActive.setSelection( selected );
    
    final Text text = new Text( group, SWT.BORDER );
    text.setText( getPreferenceStore().getString( EXTRA_OPTIONS ) );
    text.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    text.addModifyListener( new ModifyListener() {
      public void modifyText( final ModifyEvent event ) {
        getPreferenceStore().setValue( EXTRA_OPTIONS, text.getText() );
      }
    } );
    text.setEnabled( selected );
    
    cbActive.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent e ) {
        boolean selected = cbActive.getSelection();
        text.setEnabled( selected );
        getPreferenceStore().setValue( USE_EXTRA_OPTIONS, selected );
      }
    } );
  }

  private void createExecutableField( final Composite parent ) {
    String labelText = "GHC executable";
    ExecutableDialogField dlgField = new ExecutableDialogField( parent, 
                                                                labelText ) {
      @Override
      protected String createDisplayContent( final String info ) {
        return Util.queryGHCExecutable( info );
      }
    };

    dlgField.setInfo( getPreferenceStore().getString( EXECUTABLE_NAME ) );
    
    dlgField.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        getPreferenceStore().setValue( EXECUTABLE_NAME, ( String )newInfo );
      }
    } );
  }
}