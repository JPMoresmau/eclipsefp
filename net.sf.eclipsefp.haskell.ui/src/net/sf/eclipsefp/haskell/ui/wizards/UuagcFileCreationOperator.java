/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.wizards;

import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.core.uuagc.UuagcFile;
import net.sf.eclipsefp.haskell.core.uuagc.UuagcProjectManager;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * Operation for creating a new .ag file and include it in the uuagc_options file.
 * @author Alejandro Serrano
 *
 */
public class UuagcFileCreationOperator extends TemplateFileCreationOperation {

  private final boolean useHaskellSyntax;

  public UuagcFileCreationOperator( final ModuleCreationInfo info,
      final InputStream stream, final String fileExtension, final boolean useHaskellSyntax ) {
    super( info, stream, fileExtension );
    this.useHaskellSyntax = useHaskellSyntax;
  }

  @Override
  public void run( final IProgressMonitor monitor )
      throws InvocationTargetException {
    super.run( monitor );

    IFile file = getGeneratedFile();
    UuagcProjectManager mgr = new UuagcProjectManager( file.getProject() );
    mgr.initFromProject();
    UuagcFile uuagc = new UuagcFile( file.getProjectRelativePath()
        .toPortableString() );
    // Initial set of things
    uuagc.addOption( "data" );
    uuagc.addOption( "semfuns" );
    uuagc.addOption( "catas" );
    uuagc.addOption( "signatures" );
    uuagc.addOption( "pretty" );
    uuagc.addOption( "wrappers" );
    uuagc.addOption( "rename" );
    uuagc.addOption( "module \"" + getInfo().getQualifiedModuleName() + "\"" );
    if (useHaskellSyntax) {
      uuagc.addOption( "haskellsyntax" );
    }
    mgr.addElement( uuagc );
    try {
      mgr.save();
      file.getProject().refreshLocal( IResource.DEPTH_ONE, null );
    } catch( CoreException e ) {
      throw new InvocationTargetException( e );
    }
  }
}
