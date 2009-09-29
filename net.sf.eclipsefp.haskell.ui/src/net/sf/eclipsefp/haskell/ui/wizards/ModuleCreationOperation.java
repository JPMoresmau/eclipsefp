// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import java.lang.reflect.InvocationTargetException;
import net.sf.eclipsefp.haskell.core.code.ModuleCreationInfo;
import net.sf.eclipsefp.haskell.core.code.SourceFileGenerator;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;


/** <p>the operation that creates a new module in a Haskell project.</p>
  *
  * @author Leif Frenzel
  */
public class ModuleCreationOperation implements IRunnableWithProgress {

  private final ModuleCreationInfo info;
  // the file that is finally generated in this operation
  private IFile generatedFile;


  public ModuleCreationOperation( final ModuleCreationInfo info ) {
    this.info = info;
  }

  IFile getGeneratedFile() {
    return generatedFile;
  }


  // interface methods of IRunnableWithProgress
  /////////////////////////////////////////////

  public void run( final IProgressMonitor monitor )
                                              throws InvocationTargetException {
    Assert.isNotNull( info );
    try {
      generatedFile = new SourceFileGenerator().createFile( monitor, info );
      HaskellUIPlugin.getDefault().getScionInstanceManager( generatedFile ).buildProject( false );

    } catch( CoreException ex ) {
      throw new InvocationTargetException( ex );
    }
  }
}