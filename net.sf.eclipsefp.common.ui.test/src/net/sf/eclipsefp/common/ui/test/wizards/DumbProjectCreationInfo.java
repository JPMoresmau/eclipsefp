package net.sf.eclipsefp.common.ui.test.wizards;

import net.sf.eclipsefp.common.ui.wizards.ProjectCreationInfo;
import net.sf.eclipsefp.haskell.core.project.HaskellNature;

public class DumbProjectCreationInfo extends ProjectCreationInfo {

	DumbProjectCreationInfo() {
	    setProjectNatures(new String[] { HaskellNature.NATURE_ID });
	    setDirectories(new String[ 0 ]);
	}

}
