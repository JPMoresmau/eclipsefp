package net.sf.eclipsefp.haskell.ui.internal.wizards;


import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import org.eclipse.jface.resource.ImageDescriptor;

public abstract class BuildTargetWizardPage extends ValidatedWizardPage {

  private IHaskellProject fProject;

  public BuildTargetWizardPage( String pageName ) {
    super( pageName );
  }

  public BuildTargetWizardPage( String pageName, String title,
      ImageDescriptor titleImage ) {
    super( pageName, title, titleImage );
  }

  public IHaskellProject getProject() {
    return fProject;
  }

  public void setProject( IHaskellProject project ) {
    fProject = project;
  }

}