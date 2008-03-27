// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.DefaultHaskellCompiler;
import net.sf.eclipsefp.haskell.core.compiler.IHaskellCompiler;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.core.project.IImportLibrary;
import net.sf.eclipsefp.haskell.core.project.ImportLibrariesList;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;

/**
 * <p>
 * Implementation for a Haskell project info. Contains information about source
 * paths etc. This is essentially what is stored in the .hsproject file in the
 * project directory.
 * </p>
 *
 * @author Leif Frenzel
 */
public final class HaskellProject implements IHaskellProject {

	private final IProject project;
	private final Set<IPath> sourcePaths = new HashSet<IPath>();
	private final Set<IPath> targetNames = new HashSet<IPath>();
	private String outputPath = ""; //$NON-NLS-1$
	private final String binPath = ""; //$NON-NLS-1$
	private IHaskellCompiler compiler;

	public HaskellProject( final IProject project ) {
    compiler = CompilerManager.getInstance().getCompiler();
    this.project = project;
  }


	// interface methods of IHaskellProject
	// /////////////////////////////////////

	public IProject getResource() {
		return project;
	}

	public Set<IPath> getSourcePaths() {
    return Collections.unmodifiableSet( sourcePaths );
  }

	public Set<IPath> getTargetNames() {
	  return Collections.unmodifiableSet( targetNames );
	}

	public IPath getOutputPath() {
		return getProjectRelativePath(outputPath);
	}

	public IPath getBinPath() {
		return getProjectRelativePath(binPath);
	}

	public IImportLibrary[] getImportLibraries() {
    ImportLibrariesList list = new ImportLibrariesList( getResource() );
    return list.getAll();
  }

	// interface methods of IAdaptable
	// ////////////////////////////////

	public Object getAdapter( final Class required ) {
    Object result = null;
    if( required == IResource.class ) {
      result = getResource();
    }
    return result;
  }

	// internal setters
	// /////////////////

	public void addSourcePath( final String sourcePath ) {
    check( sourcePath );

    String name = IHaskellProject.PROPERTY_SOURCE_PATH;
    ProjectPropertiesEvent event = new ProjectPropertiesEvent( this, name );
    event.setOldValue( getSourcePaths() );

    sourcePaths.add( getProjectRelativePath( sourcePath ) );

    event.setNewValue( getSourcePaths() );
    HaskellProjectManager.broadcast( event );
  }

  public void setOutputPath( final String outputPath ) {
    check( outputPath );

    String name = IHaskellProject.PROPERTY_OUTPUT_PATH;
    ProjectPropertiesEvent event = new ProjectPropertiesEvent( this, name );
    event.setOldValue( getOutputPath() );

    this.outputPath = outputPath;

    event.setNewValue( getOutputPath() );
    HaskellProjectManager.broadcast( event );
  }

  public void addTargetName( final IPath targetName ) {
    String name = IHaskellProject.PROPERTY_TARGET_NAME;
    ProjectPropertiesEvent event = new ProjectPropertiesEvent( this, name );
    event.setOldValue( getTargetNames() );

    targetNames.add( targetName );

    event.setNewValue( getTargetNames() );
    HaskellProjectManager.broadcast( event );
  }

  public IContainer getSourceFolder() {
    IContainer result = project;
    if( !getSourcePaths().isEmpty() ) {
      IPath sourcePath = getSourcePaths().iterator().next();
      if( !sourcePath.equals( project.getProjectRelativePath() ) ) {
        result = project.getFolder( sourcePath );
      }
    }
    return result;
  }

  public IHaskellCompiler getCompiler() {
    return compiler;
  }

	public void setCompiler( final IHaskellCompiler comp ) {
    compiler = ( comp == null ) ? new DefaultHaskellCompiler() : comp;
  }

  public void compile( final IFile file ) {
    getCompiler().compile( file );
  }

	// helping methods
  ///////////////////

  private IPath getProjectRelativePath( final String whichPath ) {
    IPath result;
    if( "".equals( whichPath ) ) { //$NON-NLS-1$
      result = project.getProjectRelativePath();
    } else {
      result = project.getFolder( whichPath ).getProjectRelativePath();
    }
    return result;
  }

  private void check( final String candidate ) {
    if( candidate == null ) {
      throw new IllegalArgumentException();
    }
  }
}