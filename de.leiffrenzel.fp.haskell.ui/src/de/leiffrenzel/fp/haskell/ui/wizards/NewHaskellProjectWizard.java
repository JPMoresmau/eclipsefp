// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.wizards;

import net.sf.eclipsefp.common.ui.wizards.ProjectCreationWizard;

/** <p>The wizard for the creation of new projects with the Haskell nature in 
  * the workspace.</p>
  * 
  * @author Leif Frenzel
  */
public class NewHaskellProjectWizard extends ProjectCreationWizard {

  public NewHaskellProjectWizard() {
    super( new HaskellProjectCreationInfo() );
  }
}
