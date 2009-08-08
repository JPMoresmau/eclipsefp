package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model;

import java.util.ArrayList;
import java.util.List;
import net.sf.eclipsefp.haskell.core.project.HaskellProjectManager;
import net.sf.eclipsefp.haskell.core.project.IBuildTarget;
import net.sf.eclipsefp.haskell.core.project.IHaskellProject;
import net.sf.eclipsefp.haskell.ui.internal.util.UITexts;
import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;
import org.eclipse.core.resources.IProject;

/**
 * A tree node in the Project Explorer, under the main IProject node,
 * that contains all build targets for this project.
 *
 * TODO TtC allow right-click and add
 *
 * @author Thomas ten Cate
 */
public class BuildTargets implements ITreeElement {

  private final IProject project;

  public BuildTargets(final IProject project) {
    this.project = project;
  }

  public List<IBuildTarget> getChildren() {
    IHaskellProject hsProject = HaskellProjectManager.get( project );
    return new ArrayList<IBuildTarget>(hsProject.getTargets());
  }

  public String getImageKey() {
    return IImageNames.BUILD_TARGETS;
  }

  public Object getParent() {
    return project;
  }

  public String getText() {
    return UITexts.buildTargets_text;
  }

}
