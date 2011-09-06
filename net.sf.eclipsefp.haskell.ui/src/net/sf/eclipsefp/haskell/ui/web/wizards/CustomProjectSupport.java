/* Code taken from
 * http://cvalcarcel.wordpress.com/2009/07/26/writing-an-eclipse-plug-in-part-4-create-a-custom-project-in-eclipse-new-project-wizard-the-behavior/
 */

package net.sf.eclipsefp.haskell.ui.web.wizards;

import java.net.URI;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;


public class CustomProjectSupport {

  /**
   * For this marvelous project we need to: - create the default Eclipse project
   * - add the custom project nature - create the folder structure
   *
   * @param projectName
   * @param location
   * @param natureId
   * @return
   */
  public static IProject createExampleProject(final String projectName, final URI location) {
      Assert.isNotNull(projectName);
      Assert.isTrue(projectName.trim().length() > 0);

      IProject project = createBaseProject(projectName, location);
      try {
          addNature(project, HaskellNature.NATURE_ID);
          String[] paths = { "parent/child1-1/child2", "parent/child1-2/child2/child3" }; //$NON-NLS-1$ //$NON-NLS-2$
          addToProjectStructure(project, paths);
      } catch (CoreException e) {
          e.printStackTrace();
          project = null;
      }

      return project;
  }

  public static IProject createHaskellProject(final String projectName, final URI location) {
    Assert.isNotNull(projectName);
    Assert.isTrue(projectName.trim().length() > 0);

    IProject project = createBaseProject(projectName, location);
    try {
        addNature(project, HaskellNature.NATURE_ID);
        // No special paths added
    } catch (CoreException e) {
        e.printStackTrace();
        project = null;
    }

    return project;
}

  /**
   * Just do the basics: create a basic project.
   *
   * @param location
   * @param projectName
   */
  public static IProject createBaseProject(final String projectName, final URI location) {
      // it is acceptable to use the ResourcesPlugin class
      IProject newProject = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);

      if (!newProject.exists()) {
          URI projectLocation = location;
          IProjectDescription desc = newProject.getWorkspace().newProjectDescription(newProject.getName());
          if (location != null && ResourcesPlugin.getWorkspace().getRoot().getLocationURI().equals(location)) {
              projectLocation = null;
          }

          desc.setLocationURI(projectLocation);
          try {
              newProject.create(desc, null);
              if (!newProject.isOpen()) {
                  newProject.open(null);
              }
          } catch (CoreException e) {
              e.printStackTrace();
          }
      }

      return newProject;
  }

  public static void createFolder( final IFolder folder ) throws CoreException {
    IContainer parent = folder.getParent();
    if( parent instanceof IFolder ) {
      createFolder( ( IFolder )parent );
    }
    if( !folder.exists() ) {
      folder.create( false, true, null );
    }
  }

  /**
   * Create a folder structure with a parent root, overlay, and a few child
   * folders.
   *
   * @param newProject
   * @param paths
   * @throws CoreException
   */
  public static void addToProjectStructure( final IProject newProject, final String[] paths )
      throws CoreException {
    for( String path: paths ) {
      IFolder etcFolders = newProject.getFolder( path );
      createFolder( etcFolders );
    }
  }

  public static void addNature( final IProject project, final String natureId ) throws CoreException {
    if( !project.hasNature( natureId ) ) {
      IProjectDescription description = project.getDescription();
      String[] prevNatures = description.getNatureIds();
      String[] newNatures = new String[ prevNatures.length + 1 ];
      System.arraycopy( prevNatures, 0, newNatures, 0, prevNatures.length );
      newNatures[ prevNatures.length ] = natureId;
      description.setNatureIds( newNatures );

      IProgressMonitor monitor = null;
      project.setDescription( description, monitor );
    }
  }

}
