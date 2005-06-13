// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.sf.eclipsefp.common.ui.dialog.DirectoryDialogField;
import net.sf.eclipsefp.common.ui.dialog.ExecutableDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.haskell.haddock.core.HaddockInfo;
import net.sf.eclipsefp.haskell.haddock.core.HaddockUtil;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

/** <p>the page to select source files on which Haddock should be run.</p>
  * 
  * @author Leif Frenzel
  */
class SelectionPage extends HaddockExportWizardPage {

  SelectionPage( final HaddockInfo info ) {
    super( "SelectionPage", info );
    setTitle( "Basic settings" );
    String description =   "Choose source files and set the output folder and " 
                         + "the Haddock executable.";
    setDescription( description );
  }

  
  // interface methods of IWizardPage
  ///////////////////////////////////
  
  public void createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 1, false ) );
    composite.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    createCommandBlock( composite );

    Group sourceGroup = new Group( createWrapper( composite ), SWT.NONE );
    sourceGroup.setLayout( new GridLayout( 1, false ) );
    sourceGroup.setText( "Source files" );
    sourceGroup.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    createSelectionBlock( sourceGroup );
    
    createOutputDirBlock( composite );
    
    setControl( composite );
  }

  
  // UI creation methods
  //////////////////////

  private void createCommandBlock( final Composite composite ) {
    String labelText = "Haddock executable";
    ExecutableDialogField dlgField = new ExecutableDialogField( composite, 
                                                                labelText ) {
      protected String createDisplayContent( final String info ) {
        return HaddockUtil.queryHaddockExecutable( info );
      }
    };
    dlgField.setInfo( getInfo().getExecutable() );
    
    dlgField.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        getInfo().setExecutable( ( String )newInfo );
      }
    } );
    dlgField.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
  }
  
  private void createSelectionBlock( final Composite composite ) {
    CheckBoxTreeAndListLP labelProvider = new CheckBoxTreeAndListLP();
    int width = convertWidthInCharsToPixels( 60 );
    int height = convertHeightInCharsToPixels( 10 );
    final CheckboxTreeAndListViewer ctalViewer 
      = new CheckboxTreeAndListViewer( composite, 
                                       this, 
                                       new CheckboxTreeCP(), 
                                       labelProvider, 
                                       new CheckboxListCP(), 
                                       labelProvider, 
                                       width, 
                                       height );
    ctalViewer.addCheckStateListener( new ICheckStateListener() {
      public void checkStateChanged( final CheckStateChangedEvent event ) {
        List list = new ArrayList();
        Iterator it = ctalViewer.getAllCheckedListItems();
        while( it.hasNext() ) {
          IFile file = ( IFile )it.next();
          list.add( file.getLocation().toOSString() );
        }
        String[] fileList = new String[ list.size() ];
        list.toArray( fileList );
        getInfo().setFileList( fileList );
      }
    } );    
  }

  private void createOutputDirBlock( final Composite composite ) {
    String labelText = "Output directory";
    DirectoryDialogField ddf = new DirectoryDialogField( composite, labelText );
    ddf.setInfo( getInfo().getOutputDir() );
    ddf.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        getInfo().setOutputDir( ( String )newInfo );
        validate();
      }
    } );
    ddf.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
  }


  // helping methods
  //////////////////
  
  private void validate() {
    List statusList = new ArrayList();
    statusList.add( validateDirectory( getInfo().getOutputDir(), 
                                       "Output directory" ) );
    statusList.add( validateNonEmptyString( getInfo().getExecutable(), 
                                            "Haddock executable" ) );
    apply( statusList );
  }
}
