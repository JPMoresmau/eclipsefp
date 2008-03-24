// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.wizards;

import java.io.File;
import net.sf.eclipsefp.haskell.core.internal.project.IProjectCreationOperationExtraOp;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IImportWizard;

/** <p>wizard for importing a cabalized package into a new Haskell project
  * in the workspace.</p>
  *
  * @author Leif Frenzel
  */
public class CabalPackageImportWizard extends ProjectCreationWizard
                                      implements IImportWizard {

  public CabalPackageImportWizard() {
    super( new HaskellProjectCreationOperation() );
  }


  // interface methods of ProjectCreationWizard
  /////////////////////////////////////////////

  @Override
  public boolean performFinish() {
    getOperation().setExtraOperation( new IProjectCreationOperationExtraOp() {
      public void run( final IProject project,
                       final IProgressMonitor mo ) throws CoreException {
        mo.beginTask( "Importing files", 100 );
        try {
          String loc = ( ( CabalPackageImportWP )page ).getArchiveLocation();
          File sourceFile = new File( loc );
          Extractor.extract( sourceFile, project );
        } finally {
          mo.done();
        }
      }
    } );
    return super.performFinish();
  }

  @Override
  public void addPages() {
    page = new CabalPackageImportWP( "CabalPackageImportWP" ); //$NON-NLS-1$
    page.setTitle( getPageTitle() );
    page.setDescription( getPageDescription() );
    addPage( page );
  }

  @Override
  protected ImageDescriptor getBannerImage() {
    String key = IImageNames.IMPORT_CABAL_PACKAGE;
    return HaskellUIImages.getImageDescriptor( key );
  }

  @Override
  protected String getPageDescription() {
    return UITexts.cabalPackageImportWizard_pageDesc;
  }

  @Override
  protected String getPageTitle() {
    return UITexts.cabalPackageImportWizard_pageTitle;
  }
}
