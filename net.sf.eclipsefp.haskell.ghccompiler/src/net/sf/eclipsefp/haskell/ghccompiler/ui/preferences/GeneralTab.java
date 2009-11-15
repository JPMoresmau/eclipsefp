// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.haskell.ghccompiler.core.IGhcParameters;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;
import net.sf.eclipsefp.haskell.ghccompiler.ui.internal.util.UITexts;
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
    composite.setLayout( new GridLayout( 1, false ) );

    createExtraOptionsField( composite );

    /*String text = UITexts.ghciTab_options;
    BooleanDialogField fdGHCOptions = new BooleanDialogField( composite, text );
    fdGHCOptions.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        boolean selected = ( ( Boolean )newInfo ).booleanValue();
        getPreferenceStore().setValue( GHCI_USES_GHC_OPTIONS, selected );
      }
    } );
    fdGHCOptions.setInfo( getFromStore( GHCI_USES_GHC_OPTIONS ) );

    String txt2 = UITexts.ghciTab_srcFolders;
    BooleanDialogField fdSrcFolders = new BooleanDialogField( composite, txt2 );
    fdSrcFolders.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        boolean selected = ( ( Boolean )newInfo ).booleanValue();
        getPreferenceStore().setValue( GHCI_SOURCE_FOLDERS, selected );
      }
    } );
    fdSrcFolders.setInfo( getFromStore( GHCI_SOURCE_FOLDERS ) );

    Label lblNote = new Label( composite, SWT.WRAP );
    lblNote.setText( UITexts.ghciTab_note );
    */
    return composite;
  }


  // helping methods
  //////////////////

  private void createExtraOptionsField( final Composite parent ) {
    Composite wrapper = new Composite( parent, SWT.NONE );
    wrapper.setLayout( new GridLayout( 1, false ) );

    Group group = new Group( wrapper, SWT.NONE );
    group.setLayout( new GridLayout() );
    group.setText( UITexts.generalTab_extra );
    group.setLayoutData( new GridData( GridData.FILL_HORIZONTAL) );

    final Button cbActive = new Button( group, SWT.CHECK );
    cbActive.setText( UITexts.generalTab_extraMsg );
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

  /*private Boolean getFromStore( final String name ) {
    boolean value = getPreferenceStore().getBoolean( name );
    return ( value ) ? Boolean.TRUE : Boolean.FALSE;
  }*/
}