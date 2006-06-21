// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.wizards;

import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;

/** <p>wizard page for the wizard that creates a new fp project.</p>
 * 
 * @author Leif Frenzel
 */
public class ProjectCreationWizardPage extends WizardNewProjectCreationPage {

  public ProjectCreationWizardPage(String title, String description) {
    super( "ProjectCreationWizardPage" );
    setTitle(title);
    setDescription(description);
  }
  
}