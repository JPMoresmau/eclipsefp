package net.sf.eclipsefp.haskell.core.partitioned.happy;

import net.sf.eclipsefp.haskell.core.internal.project.AbstractAddNatureActionDelegate;

/**
 * Delegate that add the 'happy' builder to a project.
 *
 * @author Alejandro Serrano
 */
public class AddNatureActionDelegate extends AbstractAddNatureActionDelegate {

  public AddNatureActionDelegate() {
    // Do nothing
  }

  @Override
  protected String getBuilderID() {
    return HappyBuilder.BUILDER_ID;
  }

}
