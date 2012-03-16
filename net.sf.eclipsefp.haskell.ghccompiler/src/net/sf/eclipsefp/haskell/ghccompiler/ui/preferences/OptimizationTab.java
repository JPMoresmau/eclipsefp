// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.dialog.DialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;
import net.sf.eclipsefp.haskell.ghccompiler.ui.preferences.dialog.LevelSelectionDialogField;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;

/** <p>Tab for general compiler optimizations on the ghc preference page.</p>
  *
  * @author Leif Frenzel
  */
public class OptimizationTab extends GhcCompilerTab  {
  private DialogField dlgField;

  public OptimizationTab( final IPreferenceStore store ) {
    super( store );
  }

  // interface methods of Tab
  ///////////////////////////

  @Override
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

   // Group grpGeneral = createGeneralGroup( composite );
    dlgField = new LevelSelectionDialogField( composite );
    initLevelSelection( dlgField );

    //Group grpIndividual = createIndividualGroup( composite );
    //createIndividualContents( grpIndividual );

    return composite;
  }


  // UI creation
  //////////////

  private void initLevelSelection( final DialogField dlgField ) {
    String key = IGhcPreferenceNames.OPTIMIZATION_LEVEL;
    Integer info = new Integer( getPreferenceStore().getInt( key ) );
    dlgField.setInfo( info );

    dlgField.addDialogFieldListener( new IDialogFieldListener() {
      @Override
      public void infoChanged( final Object newInfo ) {
        String key = IGhcPreferenceNames.OPTIMIZATION_LEVEL;
        int value = ( ( Integer )newInfo ).intValue();
        getPreferenceStore().setValue( key, value );
      }
    } );
  }

  /*  private Group createGeneralGroup( final Composite composite ) {
    Group result = new Group( composite, SWT.SHADOW_ETCHED_IN );
    result.setText( ScionText.optimizationTab_general );
    result.setLayout( new GridLayout( 1, false ) );
    return result;
  }

 private Group createIndividualGroup( final Composite composite ) {
    Group result = new Group( composite, SWT.SHADOW_ETCHED_IN );
    result.setText( ScionText.optimizationTab_individual );
    result.setLayout( new GridLayout( 1, false ) );
    return result;
  }

  private void createIndividualContents( final Group grpIndividual ) {
    Label label = new Label( grpIndividual, SWT.WRAP );
    label.setText( ScionText.optimizationTab_individualInfo );

    createBooleanField( grpIndividual, OPT_EXCESS_PRECISION );
    createBooleanField( grpIndividual, OPT_IGNORE_ASSERTS );
    createBooleanField( grpIndividual, OPT_NO_STRICTNESS );
    createBooleanField( grpIndividual, OPT_NO_CPR );
    createBooleanField( grpIndividual, OPT_UNBOX_STRICT_FIELDS );
  }*/

  @Override
  public void propertyChange( final PropertyChangeEvent event ) {
    String key = IGhcPreferenceNames.OPTIMIZATION_LEVEL;
    Integer info = new Integer( getPreferenceStore().getInt( key ) );
    dlgField.setInfo( info );

  }
}