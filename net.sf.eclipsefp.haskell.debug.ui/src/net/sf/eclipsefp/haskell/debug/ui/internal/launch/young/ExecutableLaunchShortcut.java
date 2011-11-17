package net.sf.eclipsefp.haskell.debug.ui.internal.launch.young;

import org.eclipse.core.resources.IResource;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.ui.ILaunchShortcut2;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;


public class ExecutableLaunchShortcut extends LaunchOperation implements
    ILaunchShortcut2 {

  public void launch( ISelection selection, String mode ) {
    // TODO Auto-generated method stub

  }

  public void launch( IEditorPart editor, String mode ) {
    // TODO Auto-generated method stub

  }

  public ILaunchConfiguration[] getLaunchConfigurations( ISelection selection ) {
    // TODO Auto-generated method stub
    return null;
  }

  public ILaunchConfiguration[] getLaunchConfigurations( IEditorPart editorpart ) {
    // TODO Auto-generated method stub
    return null;
  }

  public IResource getLaunchableResource( ISelection selection ) {
    // TODO Auto-generated method stub
    return null;
  }

  public IResource getLaunchableResource( IEditorPart editorpart ) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  protected String getConfigTypeName() {
    // TODO Auto-generated method stub
    return null;
  }

}
