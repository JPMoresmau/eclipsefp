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
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;



/** <p>wizard page for the wizard that creates a new fp project.</p>
 * 
 * @author Leif Frenzel
 */
public class ProjectCreationWizardPage extends WizardNewProjectCreationPage {

  public ProjectCreationWizardPage( final ProjectCreationInfo info ) {
    super( "ProjectCreationWizardPage" );
    setTitle( info.getPageTitle() );
    setDescription( info.getPageDescription() );
  }
  
}