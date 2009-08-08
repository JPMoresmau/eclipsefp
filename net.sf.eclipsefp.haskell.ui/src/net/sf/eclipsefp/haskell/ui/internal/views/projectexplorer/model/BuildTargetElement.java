package net.sf.eclipsefp.haskell.ui.internal.views.projectexplorer.model;

import java.util.Collections;
import java.util.List;
import net.sf.eclipsefp.haskell.core.project.IBuildTarget;
import net.sf.eclipsefp.haskell.ui.internal.views.common.ITreeElement;
import net.sf.eclipsefp.haskell.ui.util.IImageNames;

/**
 * A specific build target in the Project Explorer,
 * under the {@link BuildTargets} node.
 *
 * TODO TtC allow right-click and run, delete, rename, configure, ...; double-click to configure
 *
 * @author Thomas ten Cate
 */
public abstract class BuildTargetElement implements ITreeElement {

  private final IBuildTarget buildTarget;
  private final Object parent;

  public BuildTargetElement(final IBuildTarget buildTarget, final Object parent) {
    this.buildTarget = buildTarget;
    this.parent = parent;
  }

  protected IBuildTarget getBuildTarget() {
    return buildTarget;
  }

  public Object getParent() {
    return parent;
  }

  public List<?> getChildren() {
    return Collections.emptyList();
  }

  public String getText() {
    return buildTarget.getPlatformPath().toPortableString();
  }

  public String getImageKey() {
    return IImageNames.BUILD_TARGET;
  }

}
