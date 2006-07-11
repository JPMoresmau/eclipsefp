// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.dialog.DialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.haskell.ghccompiler.core.IGhcParameters;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;
import net.sf.eclipsefp.haskell.ghccompiler.ui.preferences.dialog.LevelSelectionDialogField;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

/** <p>Tab for general compiler optimizations on the ghc preference page.</p>
  * 
  * @author Leif Frenzel
  */
public class OptimizationTab extends GhcCompilerTab implements IGhcParameters {
  
  public OptimizationTab( final IPreferenceStore store ) {
    super( store );
  }

  // interface methods of Tab
  ///////////////////////////
  
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );
    
    Group grpGeneral = createGeneralGroup( composite );
    DialogField dlgField = new LevelSelectionDialogField( grpGeneral );
    initLevelSelection( dlgField );

    Group grpIndividual = createIndividualGroup( composite );
    createIndividualContents( grpIndividual );
    
    return composite;
  }

  
  // UI creation
  //////////////

  private void initLevelSelection( final DialogField dlgField ) {
    String key = IGhcPreferenceNames.OPTIMIZATION_LEVEL;
    Integer info = new Integer( getPreferenceStore().getInt( key ) ); 
    dlgField.setInfo( info );
    
    dlgField.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        String key = IGhcPreferenceNames.OPTIMIZATION_LEVEL;
        int value = ( ( Integer )newInfo ).intValue();
        getPreferenceStore().setValue( key, value );
      }
    } );
  }

  private Group createGeneralGroup( final Composite composite ) {
    Group result = new Group( composite, SWT.SHADOW_ETCHED_IN );
    result.setText( "General Optimization" );
    result.setLayout( new GridLayout( 1, false ) );
    return result;
  }
  
  private Group createIndividualGroup( final Composite composite ) {
    Group result = new Group( composite, SWT.SHADOW_ETCHED_IN );
    result.setText( "Individual Optimizations" );
    result.setLayout( new GridLayout( 1, false ) );
    return result;
  }
  
  private void createIndividualContents( final Group grpIndividual ) {
    Label label = new Label( grpIndividual, SWT.WRAP );
    String info =   "These are individual optimization settings."
                  + "\n(more of them are on the next tab).";
    label.setText( info );
    
    createBooleanField( grpIndividual, OPT_EXCESS_PRECISION );
    createBooleanField( grpIndividual, OPT_IGNORE_ASSERTS );
    createBooleanField( grpIndividual, OPT_NO_STRICTNESS );
    createBooleanField( grpIndividual, OPT_NO_CPR );
    createBooleanField( grpIndividual, OPT_UNBOX_STRICT_FIELDS );    
  }  
}