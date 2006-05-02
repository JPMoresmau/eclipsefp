// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.WizardPage;

import net.sf.eclipsefp.haskell.ui.util.DefaultStatus;
import net.sf.eclipsefp.haskell.ui.util.StatusUtil;

/** <p>a wizard page that provides status presentation facilities for
  * subclasses.</p>
  * 
  * @author Leif Frenzel
  */
public abstract class StatusWizardPage extends WizardPage {

  private IStatus currentStatus;
  private boolean pageVisible;

  public StatusWizardPage( final String name ) {
    super( name );
    pageVisible = false;
    currentStatus = new DefaultStatus();
  }
    
  
  // interface methods of WizardPage
  //////////////////////////////////
  
  public void setVisible( final boolean visible ) {
    super.setVisible( visible );
    pageVisible = visible;
    // policy: wizards are not allowed to come up with an error message
    if( visible && currentStatus.matches( IStatus.ERROR ) ) {
      currentStatus = new DefaultStatus();
      ( ( DefaultStatus )currentStatus ).setError( "" );
    }
    updateStatus( currentStatus );
  }

  protected void updateStatus( final IStatus[] status ) {
    updateStatus( StatusUtil.getMostSevere( status ) );
  }

  
  // helping methods
  //////////////////

  private void updateStatus( final IStatus status ) {
    currentStatus = status;
    setPageComplete( !status.matches( IStatus.ERROR ) );
    if( pageVisible ) {
      StatusUtil.applyToStatusLine( this, status );
    }
  }
}