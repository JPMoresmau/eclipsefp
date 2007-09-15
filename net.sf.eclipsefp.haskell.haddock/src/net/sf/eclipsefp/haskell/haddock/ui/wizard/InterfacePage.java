// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;

import java.util.ArrayList;
import java.util.List;

import net.sf.eclipsefp.common.ui.dialog.FileDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.haskell.haddock.core.HaddockInfo;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

/** <p>page for optionally setting options that can be used to link several
  * documentations in one.</p>
  *
  * @author Leif Frenzel
  */
class InterfacePage extends HaddockExportWizardPage {

  InterfacePage( final HaddockInfo info ) {
    super( "InterfacePage", info );
    setTitle( "Combinig documentation" );
    String description =   "Additional settings used for combining multiple\n" 
                         + "sets of generated documentation.";
    setDescription( description );
  }

  
  // interface methods of IWizardPage
  ///////////////////////////////////
  
  public void createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 2, false ) );
    composite.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    createPackageBlock( composite );
    Group interfacesGroup = new Group( composite, SWT.NONE );
    interfacesGroup.setText( "Haddock interfaces" );
    interfacesGroup.setLayout( new GridLayout( 1, false ) );
    GridData gd = new GridData( GridData.FILL_BOTH );
    gd.horizontalSpan = 2;
    interfacesGroup.setLayoutData( gd );
    createDumpInterfaceBlock( interfacesGroup );
    createReadInterfacesBlock( interfacesGroup );
    
    createIndexBlock( composite );
    createContentsBlock( composite );
    
    setControl( composite );
  }
  
  
  // UI creation methods
  //////////////////////
  
  private void createPackageBlock( final Composite composite ) {
    final Button cbPackage = createCheckBox( composite, "Package name" );
    cbPackage.setSelection( getInfo().isUsePackageName() );
    
    final Text txtPackage = new Text( composite, SWT.BORDER );
    txtPackage.setText( getInfo().getPackageName() );
    txtPackage.setEnabled( getInfo().isUsePackageName() );
    txtPackage.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    
    cbPackage.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        boolean selected = cbPackage.getSelection();
        txtPackage.setEnabled( selected );
        getInfo().setUsePackageName( selected );
      }
    } );
    
    txtPackage.addModifyListener( new ModifyListener() {
      public void modifyText( final ModifyEvent evt ) {
        getInfo().setPackageName( txtPackage.getText() );
      }
    } );
  }
  
  private void createDumpInterfaceBlock( final Composite composite ) {
    final Button cbDump = createCheckBox( composite, "Dump an interface file" );
    cbDump.setSelection( getInfo().isDumpInterface() );
    cbDump.setLayoutData( createSpanData() );
    
    String label = "File name";
    String[] filter = new String[] { "*" };
    final FileDialogField fdf = new FileDialogField( composite, label, filter );
    fdf.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        getInfo().setDumpInterfaceFile( ( String )newInfo );
        validate();
      }
    } );
    fdf.setLayoutData( createSpanData() );
    fdf.setInfo( getInfo().getDumpInterfaceFile() );
    fdf.setEnabled( getInfo().isDumpInterface() );
  
    cbDump.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        boolean selected = cbDump.getSelection();
        fdf.setEnabled( selected );
        getInfo().setDumpInterface( selected );
        validate();
      }
    } );
  }

  private void createReadInterfacesBlock( final Composite composite ) {
    final Button cbRead = createCheckBox( composite, "Read interface file(s)" );
    cbRead.setSelection( getInfo().isReadInterfaces() );
    cbRead.setLayoutData( createSpanData() );

    final InterfaceListBlock ilBlock = new InterfaceListBlock( composite, 
                                                               getInfo() );
    ilBlock.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    ilBlock.setEnabled( getInfo().isReadInterfaces() );

    cbRead.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        boolean selected = cbRead.getSelection();
        ilBlock.setEnabled( selected );
        getInfo().setReadInterfaces( selected );
      }
    } );
  }

  private void createIndexBlock( final Composite composite ) {
    final Button cbIndex = createCheckBox( composite, "Link to index (URL)" );
    cbIndex.setSelection( getInfo().isUseIndexURL() );

    final Text txtIndex = createText( composite, 
                                      getInfo().getIndexURL(),
                                      getInfo().isUseIndexURL() );
    
    cbIndex.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        boolean selected = cbIndex.getSelection();
        txtIndex.setEnabled( selected );
        getInfo().setUseIndexURL( selected );
        validate();
      }
    } );
    
    txtIndex.addModifyListener( new ModifyListener() {
      public void modifyText( final ModifyEvent evt ) {
        getInfo().setIndexURL( txtIndex.getText() );
        validate();
      }
    } );
  }

  private void createContentsBlock( final Composite composite ) {
    final Button cbContents = createCheckBox( composite, 
                                              "Link to contents (URL)" );
    cbContents.setSelection( getInfo().isUseContentsURL() );
    
    final Text txtContents = createText( composite, 
                                         getInfo().getContentsURL(),
                                         getInfo().isUseContentsURL() );
    
    cbContents.addSelectionListener( new SelectionAdapter() {
      @Override
      public void widgetSelected( final SelectionEvent evt ) {
        boolean selected = cbContents.getSelection();
        txtContents.setEnabled( selected );
        getInfo().setUseContentsURL( selected );
        validate();
      }
    } );
    
    txtContents.addModifyListener( new ModifyListener() {
      public void modifyText( final ModifyEvent evt ) {
        getInfo().setContentsURL( txtContents.getText() );
        validate();
      }
    } );
  }
  
  private Text createText( final Composite composite, 
                           final String text,
                           final boolean enabled ) {
    final Text result = new Text( composite, SWT.BORDER );
    result.setText( text );
    result.setEnabled( enabled );
    result.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    return result;
  }

  
  // helping methods
  //////////////////
  
  private void validate() {
    List statusList = new ArrayList();
    if( getInfo().isUseContentsURL() ) {
      statusList.add( validateURL( getInfo().getContentsURL() ) );
    }
    if( getInfo().isUseIndexURL() ) {
      statusList.add( validateURL( getInfo().getIndexURL() ) );
    }
    apply( statusList );
  }
}