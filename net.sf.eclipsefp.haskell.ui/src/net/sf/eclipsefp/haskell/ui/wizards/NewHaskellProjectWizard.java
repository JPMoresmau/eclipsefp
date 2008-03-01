// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.common.ui.wizards.ProjectCreationWizard;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.jface.resource.ImageDescriptor;

/**
 * <p>
 * The wizard for the creation of new projects with the Haskell nature in the
 * workspace.
 * </p>
 *
 * @author Leif Frenzel
 */
public class NewHaskellProjectWizard extends ProjectCreationWizard {

	public NewHaskellProjectWizard() {
    super( new HaskellProjectCreationOperation() );
  }

  @Override
  protected ImageDescriptor getBannerImage() {
    return HaskellUIImages.getImageDescriptor( IImageNames.NEW_PROJECT );
  }

	@Override
	protected String getPageDescrition() {
		return "Create a new Haskell project in the workspace.";
	}

	@Override
	protected String getPageTitle() {
		return "Haskell project";
	}



}
