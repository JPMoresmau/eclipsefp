// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import org.eclipse.jface.resource.ImageDescriptor;

import net.sf.eclipsefp.common.ui.wizards.ProjectCreationWizard;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation;
import net.sf.eclipsefp.haskell.ui.util.HaskellUIImages;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;

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
		super(new HaskellProjectCreationOperation());
	}
	
	@Override
	protected ImageDescriptor getBannerImage() {
		//TODO make a haskell-specific image (we are copying the image for the
		//java create project wizard for the time being)
	    return HaskellUIImages.getImageDescriptor(IImageNames.NEW_PROJECT);
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
