package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model;

import net.sf.eclipsefp.haskell.core.internal.project.IExecutableBuildTarget;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;


public class ExecutableBuildTargetElement extends BuildTargetElement {

  public ExecutableBuildTargetElement( final IExecutableBuildTarget buildTarget, final Object parent ) {
    super( buildTarget, parent );
  }

  @Override
  protected IExecutableBuildTarget getBuildTarget() {
    return (IExecutableBuildTarget)super.getBuildTarget();
  }

  @Override
  public String getImageKey() {
    return IImageNames.EXECUTABLE_TARGET;
  }

  @Override
  public String getText() {
    return super.getText() + " [" + getBuildTarget().getMain() + "]";
  }

}
