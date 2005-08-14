// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.wizards;

import net.sf.eclipsefp.common.ui.CommonUIPlugin;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;



/** <p>wizard page for the wizard that creates a new fp project.</p>
 * 
 * @author Leif Frenzel
 */
public class ProjectCreationWizardPage extends WizardPage {

  private IStatus currentStatus;
  
  private boolean pageVisible;
  
  private String nameLabel;
  private String projectName;
  
  private Text textControl;
  
  public ProjectCreationWizardPage( final ProjectCreationInfo info ) {
    super( "ProjectCreationWizardPage" );
    currentStatus = createStatus( IStatus.OK, "" );
    setTitle( info.getPageTitle() );
    setDescription( info.getPageDescription() );
    nameLabel= "Project name:";
    projectName= "";
  }
  
  public void createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    GridLayout gd = new GridLayout();
    gd.numColumns = 2;
    composite.setLayout( gd );
    
    Label label = new Label( composite, SWT.LEFT );
    label.setText( nameLabel );
    label.setLayoutData( new GridData() );
    
    textControl = new Text( composite, SWT.SINGLE | SWT.BORDER );
    textControl.setText( projectName );
    textControl.addModifyListener( new ModifyListener() {
      public void modifyText( final ModifyEvent e ) {
        if( !textControl.isDisposed() ) {
          validateText( textControl.getText() );
        }
      }
    } );
    textControl.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    setControl( composite );
  }
  
  private void validateText( final String text ) {
    IWorkspace workspace = ResourcesPlugin.getWorkspace();
    IStatus status = workspace.validateName( text, IResource.PROJECT );
    if( status.isOK() ) {
      if( workspace.getRoot().getProject( text ).exists() ) {
        status = createStatus( IStatus.ERROR, "Exists already." );
      }
    }
    updateStatus( status );
    projectName = text;
  }
  
  public void setVisible( final boolean visible ) {
    super.setVisible( visible );
    pageVisible = visible;
    // policy: wizards are not allowed to come up with an error message
    if( visible && currentStatus.matches( IStatus.ERROR ) ) {
      // keep the error state, but remove the message
      currentStatus = createStatus( IStatus.ERROR, "" );
    } 
    updateStatus( currentStatus );
    if( visible ) {
      textControl.setFocus();
    }
  }
  
  
  /** <p>updates the status line and the ok button depending on the 
    * status</p> */
  private void updateStatus( final IStatus status ) {
    currentStatus = status;
    setPageComplete( !status.matches( IStatus.ERROR ) );
    if( pageVisible ) {
      applyToStatusLine( this, status );
    }
  }
  
  /**
   * Applies the status to a dialog page
   */
  private static void applyToStatusLine( final DialogPage page, 
                                         final IStatus status ) {
    String errorMessage = null;
    String warningMessage = null;
    String statusMessage = status.getMessage();
    if( statusMessage.length() > 0 ) {
      if( status.matches( IStatus.ERROR ) ) {
        errorMessage = statusMessage;
      } else if ( !status.isOK() ) {
        warningMessage = statusMessage;
      }
    }
    page.setErrorMessage( errorMessage );
    page.setMessage( warningMessage );
  }
  
  private static IStatus createStatus( final int severity, 
                                       final String message ) {
    String pluginId = CommonUIPlugin.getPluginId();
    return new Status( severity, pluginId, severity, message, null );
  }
  
  /** <p>returns the name the user has entered.</p> */
  public String getName() {
    return projectName;
  }
}