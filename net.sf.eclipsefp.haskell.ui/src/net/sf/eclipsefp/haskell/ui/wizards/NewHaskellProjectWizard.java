// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.haskell.core.project.HaskellProjectCreationOperation;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.internal.wizards.ProjectCreationWizard;
import net.sf.eclipsefp.haskell.ui.internal.wizards.ProjectModelFilesOp;
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
    getOperation().setExtraOperation( new ProjectModelFilesOp() );
  }

  @Override
  protected ImageDescriptor getBannerImage() {
    return HaskellUIImages.getImageDescriptor( IImageNames.NEW_PROJECT );
  }

	@Override
	protected String getPageDescription() {
		return UITexts.newHaskellProjectWizard_pageDesc;
	}

	@Override
	protected String getPageTitle() {
		return UITexts.newHaskellProjectWizard_pageTitle;
	}
}
