// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.project;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Vector;
import net.sf.eclipsefp.haskell.core.HaskellCorePlugin;
import net.sf.eclipsefp.haskell.core.internal.project.HaskellProject;
import net.sf.eclipsefp.haskell.core.internal.project.Parser;
import net.sf.eclipsefp.haskell.core.internal.util.Assert;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.CoreException;


/** <p>The HaskellProjectManager manages Haskell project specific information
  * for IProject resources that have the Haskell project nature. That
  * information is encapsulated by {@link IHaskellProject IHaskellProject}
  * objects managed by this HaskellProjectManager.</p>
  *
  * <p>This is a singleton to make it accessible from everywhere.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellProjectManager {

  public static final String HASKELL_PROJECT_DESCRIPTOR = ".hsproject"; //$NON-NLS-1$

  private static HaskellProjectManager _instance;

  // we're cautious here and use synchronized data structures: Hashtables
  // and Vectors

  /** contains IHaskellProject info objects (values) for project resources
    * (keys) in the workspace. */
  private final Hashtable<IResource, IHaskellProject> htHaskellProjects;
  /** contains the project property change listeners registered with
    * this HaskellProjectManager. */
  private final Vector<IProjectPropertiesListener> listeners;

  /** the singleton instance of HaskellProjectManager. Private in order
    * to ensure the singleton pattern. */
  private HaskellProjectManager() {
    htHaskellProjects = new Hashtable<IResource, IHaskellProject>();
    listeners = new Vector<IProjectPropertiesListener>();
  }

  /** <p>returns a reference to the singleton instance of
    * HaskellProjectManager. This is only package private; only some of the
    * API-implementing classes of this package have the privilege of accessing
    * this instance directly.</p> */
  static synchronized HaskellProjectManager getInstance() {
    if( _instance == null ) {
      _instance = new HaskellProjectManager();
    }
    return _instance;
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
    IHaskellProject hsProject = new HaskellProject( descFile.getProject() );
    hsProject.saveDescriptor();
    return hsProject;
  }

  private IHaskellProject readFromDescriptor( final IFile descFile ) {
    HaskellProject result = new HaskellProject( descFile.getProject() );
    new Parser( descFile, result ).read();
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