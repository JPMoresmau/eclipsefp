// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.test.util.haskell;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import junit.framework.TestCase;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;

import de.leiffrenzel.fp.haskell.core.project.HaskellNature;
import de.leiffrenzel.fp.haskell.core.project.HaskellProjectManager;
import de.leiffrenzel.fp.haskell.core.project.IHaskellProject;

/** <p>the super class for test cases that run on a Haskell project.</p>
  *
  * @author Leif Frenzel
  */
public abstract class HaskellProject_PDETestCase extends TestCase {

  private IProject project;
  
  protected IProject getProject() {
    return project;
  }
  
  protected void setUp() throws Exception {
    IWorkspaceRunnable op = new IWorkspaceRunnable() {
      public void run( final IProgressMonitor monitor ) throws CoreException {
        IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        project = root.getProject( "TestProject-1" );
        project.create( null );
        project.open( null );
        
        setHaskellNature();
        createDefaultDescriptor();
        createDefaultFolders();
      }
    };
    ResourcesPlugin.getWorkspace().run( op, null, 0, null );
  }
  
  protected void tearDown() throws Exception {
    project.delete( true, null );
  }
  
  
  // helping methods
  //////////////////
  
  private void setHaskellNature() throws CoreException {
    IProjectDescription description = project.getDescription();
    description.setNatureIds( new String[] { HaskellNature.NATURE_ID } );
    project.setDescription( description, null );
  }
  
  public void createDefaultDescriptor() throws CoreException {
    String name = HaskellProjectManager.HASKELL_PROJECT_DESCRIPTOR;
    IFile file = project.getFile( name );
    String contents = HaskellProjectManager.createDescriptorContent( "src",
                                                                     "out",
                                                                     "bin",
                                                                     "x.exe" );
    writeFile( contents, file );
  }
  
  private void writeFile( final String input, 
                          final IFile newFile ) throws CoreException {
    InputStream is = new ByteArrayInputStream( input.getBytes() );
    newFile.create( is, true, null );
  }
  
  private void createDefaultFolders() throws CoreException {
    IHaskellProject hsProject = HaskellProjectManager.get( project );
    createFolder( hsProject.getSourcePath() );
    createFolder( hsProject.getBinPath() );
    createFolder( hsProject.getOutputPath() );
  }
  
  private void createFolder( final IPath folderPath ) throws CoreException {
    IFolder folder = project.getFolder( folderPath );
    folder.create( true, true, null );
  }
  
  protected String constructName( final String folder, 
                                final String name,
                                final Object context ) {
    Package pack = context.getClass().getPackage();
    String packageName = pack.getName().replace( '.', '/' ) + "/res/";
    return packageName + folder + "/" + name;
  }
}
