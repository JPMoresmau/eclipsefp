// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Vector;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.DescriptorFile;
import net.sf.eclipsefp.haskell.core.internal.project.HaskellProject;
import net.sf.eclipsefp.haskell.core.internal.project.Parser;
import net.sf.eclipsefp.haskell.core.internal.util.Assert;
import net.sf.eclipsefp.haskell.core.internal.util.CoreTexts;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;


/** The HaskellProjectManager manages Haskell project specific information
  * for IProject resources that have the Haskell project nature. That
  * information is encapsulated by {@link IHaskellProject IHaskellProject}
  * objects managed by this HaskellProjectManager.
  *
  * This is a singleton to make it accessible from everywhere.
  *
  * @author Leif Frenzel
  */
public class HaskellProjectManager {

  public static final String HASKELL_PROJECT_DESCRIPTOR = ".hsproject"; //$NON-NLS-1$

  /** The singleton instance of HaskellProjectManager. Note that this uses the "lazy"
   *  idiom for initializing singleton instances that avoids double synchronization and
   *  other nasty hacks for thread safety (see "Bill Pugh" solution.) */
  private static class SingletonHolder {
    private static final HaskellProjectManager theInstance = new HaskellProjectManager();
  }

  // we're cautious here and use synchronized data structures: Hashtables
  // and Vectors

  /** contains IHaskellProject info objects (values) for project resources
    * (keys) in the workspace. */
  private final Hashtable<IResource, IHaskellProject> htHaskellProjects;
  /** contains the project property change listeners registered with
    * this HaskellProjectManager. */
  private final Vector<IProjectPropertiesListener> listeners;

  /** Default constructor. This is private to ensure singleton properties. */
  private HaskellProjectManager() {
    htHaskellProjects = new Hashtable<IResource, IHaskellProject>();
    listeners = new Vector<IProjectPropertiesListener>();
  }

  /** <p>returns a reference to the singleton instance of
    * HaskellProjectManager. This is only package private; only some of the
    * API-implementing classes of this package have the privilege of accessing
    * this instance directly.</p> */
  public static final HaskellProjectManager getInstance() {
    return SingletonHolder.theInstance;
  }

  /** <p>removes all information from this HaskellProjectManager. </p> */
  public static void clear() {
    getInstance().clearInternal();
  }

  /** <p>returns the IHaskellProject info object for the specified project
    * resource. The passed IProject must have the {@link HaskellNature
    * Haskell project nature}.</p> */
  public static IHaskellProject get( final IProject project ) {
    if( project == null || !project.isAccessible() ) {
      throw new IllegalArgumentException();
    }
    try {
      Assert.isTrue( project.hasNature( HaskellNature.NATURE_ID ),
                       "Project " //$NON-NLS-1$
                     + project.getName()
                     + " must have the Haskell nature." ); //$NON-NLS-1$
    } catch( CoreException cex ) {
      HaskellCorePlugin.log( "Problem when checking out project.", cex ); //$NON-NLS-1$
    }
    return getInstance().getInternal( project );
  }

  /** <p>creates the contents of a project descriptor file with the
    * specified path settings for the project.</p> */
  public static String createDescriptorContent( final String sourcePath,
                                               // final String outputPath,
                                               // final String targetName,
                                                final String compiler) {
    return DescriptorFile.createDescriptorContent( sourcePath,
                                               //    outputPath,
                                               //    targetName,
                                                   compiler);
  }

  /** <p>creates the content of a project descriptor file with empty path
    * settings.</p> */
  public static String createEmptyDescriptorContent() {
    return DescriptorFile.createEmptyDescriptorContent();
  }

  /** <p>registers the specified listener for project property changes.</p> */
  public static void addProjectPropertiesListener(
                                   final IProjectPropertiesListener listener ) {
    getInstance().addListener( listener );
  }

  /** <p>de-registers the specified listener for project property
    * changes.</p> */
  public static void removeProjectPropertiesListener(
                                   final IProjectPropertiesListener listener ) {
    getInstance().removeListener( listener );
  }

  /** <p>returns an array of all Haskell Projects in the specified
    * workspace.</p> */
  public static IHaskellProject[] getAll( final IWorkspaceRoot root ) {
    List<IHaskellProject> list = new ArrayList<IHaskellProject>();
    IProject[] projects = root.getProjects();
    for( int i = 0; i < projects.length; i++ ) {
      try {
        IProject project = projects[ i ];

        if(     project.isOpen()
             && project.hasNature( HaskellNature.NATURE_ID ) ) {
          IHaskellProject hsProject = HaskellProjectManager.get( project );
          list.add( hsProject );
        }
      } catch( CoreException ex ) {
        String msg = "Problem determining Haskell projects in the workspace."; //$NON-NLS-1$
        HaskellCorePlugin.log( msg, ex );
      }
    }
    IHaskellProject[] result = new IHaskellProject[ list.size() ];
    list.toArray( result );
    return result;
  }

  /** <p>returns an array of all Haskell Projects in the specified
   * workspace.</p> */
 public static IProject[] getAllStandard( final IWorkspaceRoot root ) {
   List<IProject> list = new ArrayList<IProject>();
   for( IProject project:root.getProjects() ) {
     try {
      if(  project.isOpen()
            && project.hasNature( HaskellNature.NATURE_ID ) ) {
         list.add( project );
       }
     } catch( CoreException ex ) {
       String msg = "Problem determining Haskell projects in the workspace."; //$NON-NLS-1$
       HaskellCorePlugin.log( msg, ex );
     }
   }
   IProject[] result = new IProject[ list.size() ];
   list.toArray( result );
   return result;
 }

  // (internal) methods used by classes of this plugin
  ////////////////////////////////////////////////////

  /** <p>notifies all listeners that have registered with the
    * HaskellProjectManager about the passed event.</p>
    *
    * <p>Note: this is only for INTERNAL use. Clients are not supposed to
    * call this method.</p>
    */
  public static void broadcast( final IProjectPropertiesEvent event ) {
    Vector<IProjectPropertiesListener> lis = getInstance().listeners;
    for( IProjectPropertiesListener li: lis ) {
      li.projectPropertyChanged( event );
    }
  }


  // helping methods
  //////////////////

  private IHaskellProject getInternal( final IProject project ) {
    if( !htHaskellProjects.containsKey( project ) ) {
      IHaskellProject newHsProject;
      IFile projectDescriptor = project.getFile( HASKELL_PROJECT_DESCRIPTOR );
      if( projectDescriptor.exists() ) {
        newHsProject = readFromDescriptor( projectDescriptor );
      } else {
        newHsProject = createNew( projectDescriptor );
      }
      htHaskellProjects.put( project, newHsProject );
    }
    return htHaskellProjects.get( project );
  }

  private IHaskellProject createNew( final IFile descFile ) {
    Job job = new Job( CoreTexts.haskellProjectManager_jobName ) {
      @Override
      protected IStatus run( final IProgressMonitor monitor ) {
        IStatus result = Status.OK_STATUS;
        try {
          byte[] contents = createEmptyDescriptorContent().getBytes();
          InputStream is = new ByteArrayInputStream( contents );
          descFile.create( is, true, null );
        } catch( CoreException cex ) {
          result = cex.getStatus();
        }
        return result;
      }
    };
    job.setRule( descFile.getProject() );
    job.schedule();
    return new HaskellProject( descFile.getProject() );
  }

  private IHaskellProject readFromDescriptor( final IFile descFile ) {
    HaskellProject result = new HaskellProject( descFile.getProject() );
    Parser.readIn( descFile, result );
    return result;
  }

  private void clearInternal() {
    htHaskellProjects.clear();
  }

  private void addListener( final IProjectPropertiesListener listener ) {
    listeners.add( listener );
  }

  private void removeListener( final IProjectPropertiesListener listener ) {
    listeners.remove( listener );
  }
}