// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.wizards;

import net.sf.eclipsefp.common.ui.wizards.ProjectCreationOperation;
import net.sf.eclipsefp.common.ui.wizards.ProjectCreationWizard;

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
		super(new HaskellProjectCreationInfo());
	}

	@Override
	protected ProjectCreationOperation createOperation() {
		return new HaskellProjectCreationOperation();
	}

}
