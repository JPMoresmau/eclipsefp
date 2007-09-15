// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.haddock.ui.wizard;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import net.sf.eclipsefp.haskell.haddock.core.HaddockInfo;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

import net.sf.eclipsefp.haskell.ui.util.DefaultStatus;
import net.sf.eclipsefp.haskell.ui.util.StatusUtil;

/** <p>the super class of all wizard pages for the Haddock Export wizard.
  * Encapsulates some common functionality.</p>
  *
  * @author Leif Frenzel
  */
abstract class HaddockExportWizardPage extends WizardPage {

  private final HaddockInfo info;
  
  public HaddockExportWizardPage( final String name, final HaddockInfo info ) {
    super( name );
    this.info = info;
  }
  
  HaddockInfo getInfo() {
    return info;
  }
  
  
  // UI creation methods
  //////////////////////
  
  Button createCheckBox( final Composite composite, final String text ) {
    Button result = new Button( composite, SWT.CHECK );
    result.setText( text );
    result.setSelection( false );
    return result;
  }  
  
  GridData createSpanData() {
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.horizontalSpan = 2;
    return gridData;
  }
  
  // QnD trick in order to get the layout neatly aligned
  Composite createWrapper( final Composite composite ) {
    Composite result = new Composite( composite, SWT.NONE );
    result.setLayout( new GridLayout() );
    result.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    return result;
  }
  
  
  // validation
  /////////////
  
  void apply( final List<IStatus> list ) {
    IStatus[] states = new IStatus[ list.size() ];
    list.toArray( states );
    IStatus mostSevere = StatusUtil.getMostSevere( states );
    StatusUtil.applyToStatusLine( this, mostSevere );
    setPageComplete( !mostSevere.matches( IStatus.ERROR ) );
  }

  IStatus validateNonEmptyString( final String text, final String what ) {
    DefaultStatus result = new DefaultStatus();
    if( text == null || text.trim().equals( "" ) ) {
      result.setError( what + " must not be empty" );
    }
    return result;
  }
  
  IStatus validateDirectory( final String text, final String what ) {
    DefaultStatus result = new DefaultStatus();
    if( text == null || text.trim().equals( "" ) ) {
      result.setError( what + " must not be empty" );
    } else {
      try {
        File file = new File( text );
        if( file.exists() && !file.isDirectory() ) {
          result.setError( "Already existing file: " + text );
        }
      } catch ( Exception ex ) {
        result.setError( "Invalid file: " + text + " - " + ex.getMessage() );
      }
    }
    return result;
  }

  IStatus validateFile( final String text, final String what ) {
    DefaultStatus result = new DefaultStatus();
    if( text == null || text.trim().equals( "" ) ) {
      result.setError( what + " must not be empty" );
    } else {
      try {
        File file = new File( text );
        if( !file.exists() ) {
          result.setError( "File does not exist: " + text );
        }
      } catch ( Exception ex ) {
        result.setError( "Invalid file: " + text + " - " + ex.getMessage() );
      }
    }
    return result;
  }
  
  IStatus validateURL( final String text ) {
    DefaultStatus result = new DefaultStatus();
    try {
      new URL( text );
    } catch( MalformedURLException maux ) {
      result.setError( "Invalid URL: " + maux.getMessage() );
    }
    return result;
  }
}
