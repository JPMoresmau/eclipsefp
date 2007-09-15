// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.preferences;

import net.sf.eclipsefp.common.ui.dialog.BooleanDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.preferences.Tab;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

/** <p>the single tab on the compiler preference page (not shown as tab, 
  * but uses the preference access mechanism of tabs).</p>
  * 
  * @author Leif Frenzel
  */
class HaskellCompilerTab extends Tab implements IPreferenceConstants {
  
  HaskellCompilerTab( final IPreferenceStore store ) {
    super( store );
  }
  
  
  // interface methods of Tab
  ///////////////////////////
  
  @Override
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 1, false ) );
    
    Group group = new Group( composite, SWT.SHADOW_ETCHED_IN );
    group.setLayout( new GridLayout( 1, false ) );
    group.setText( "Compilers" );
    createCompilerSelection( group );
    createNoteLabel( group );
    
    String text = "Show compiler output in view";
    BooleanDialogField result = new BooleanDialogField( composite, text );
    result.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        boolean selected = ( ( Boolean )newInfo ).booleanValue();
        getPreferenceStore().setValue( SHOW_COMPILER_LOG, selected );
      }
    } );
    result.setInfo( getFromStore( SHOW_COMPILER_LOG ) );
    
    return composite;
  }
  
  
  // helping methods
  //////////////////
  
  private void createCompilerSelection( final Composite composite ) {
    // TODO select the right compiler etc.
    ListViewer viewer = new ListViewer( composite );
    GridData gd = new GridData( GridData.FILL_HORIZONTAL );
    viewer.getControl().setLayoutData( gd );
    viewer.setContentProvider( new CompilerListCP() );
    viewer.setLabelProvider( new CompilerListLP() );
    Object dummyInput = new Object();
    viewer.setInput( dummyInput );
    viewer.getControl().setEnabled( false );
  }

  private void createNoteLabel( final Composite composite ) {
    Label label = new Label( composite, SWT.WRAP );
    String text =   "Note:\n\nThis is only a compiler support. The "
                  + "compilers themselves are not included\nand must be "
                  + "installed on the system. You will probably have to "
                  + "configure\nthe compiler that you select here. See "
                  + "whether it has a specific preference\npage and look "
                  + "there.";
    label.setText( text );
    label.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
  }

  private Boolean getFromStore( final String name ) {
    boolean value = getPreferenceStore().getBoolean( name );
    return ( value ) ? Boolean.TRUE : Boolean.FALSE;
  }  
}