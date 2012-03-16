// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
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
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
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
@Deprecated
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

	@Override
  public IProject getResource() {
		return project;
	}

	@Override
  public Set<IPath> getSourcePaths() {
    return Collections.unmodifiableSet( sourcePaths );
  }

	@Override
  public Set<IPath> getTargetNames() {
	  return Collections.unmodifiableSet( targetNames );
	}

	@Override
  public IPath getOutputPath() {
		return getProjectRelativePath(outputPath);
	}

	public IPath getBinPath() {
		return getProjectRelativePath(binPath);
	}

	@Override
  public IImportLibrary[] getImportLibraries() {
    ImportLibrariesList list = new ImportLibrariesList( getResource() );
    return list.getAll();
  }

	// interface methods of IAdaptable
	// ////////////////////////////////

	@Override
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

  @Override
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

  @Override
  public IHaskellCompiler getCompiler() {
    return compiler;
  }

	public void setCompiler( final IHaskellCompiler comp ) {
    compiler = ( comp == null ) ? new DefaultHaskellCompiler() : comp;
  }

  @Override
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

  @Override
  public Map<String, IFile> getModulesFile( ) {
    Set<IPath> paths = getSourcePaths();
    HashMap<String, IFile> r = new HashMap<String, IFile>();
    try {
      this.getResource().accept( new ModulesVisitor( r, paths ) );
    } catch (CoreException e) {
      r.clear();
    }
    return r;
  }

  @Override
  public IFile getModuleFile( final String module ) {
    return this.getModulesFile( ).get( module );
  }

  public class ModulesVisitor implements IResourceVisitor {

    public Map<String, IFile> elts;
    public Vector<String> possiblePrefixes;

    public ModulesVisitor( final Map<String, IFile> whereAdd,
        final Set<IPath> dirs ) {
      this.elts = whereAdd;
      this.possiblePrefixes = new Vector<String>();
      for( IPath dir: dirs ) {
        this.possiblePrefixes.add( dir.toPortableString() + "/" ); //$NON-NLS-1$
      }
    }

    @Override
    public boolean visit( final IResource resource ) {
      String path = resource.getProjectRelativePath().toString();
      if( resource instanceof IFile ) {
        IFile file = (IFile)resource;
        for( String dir: possiblePrefixes ) {
          if( path.startsWith( dir ) ) {
            String filePath = path.substring( dir.length() );
            if( filePath.endsWith( ".hs" ) ) { //$NON-NLS-1$
              String module = filePath.substring( 0, filePath.length() - 3 )
                  .replace( '/', '.' );
              this.elts.put( module, file );
            } else if( filePath.endsWith( ".lhs" ) ) { //$NON-NLS-1$
              String module = filePath.substring( 0, filePath.length() - 4 )
                  .replace( '/', '.' );
              this.elts.put( module, file );
            }
          }
        }
      }
      return true;
    }
  }
}