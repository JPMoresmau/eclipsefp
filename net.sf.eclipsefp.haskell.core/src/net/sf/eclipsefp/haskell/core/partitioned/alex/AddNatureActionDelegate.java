package net.sf.eclipsefp.haskell.core.partitioned.alex;

import net.sf.eclipsefp.haskell.core.internal.project.AbstractAddNatureActionDelegate;

/**
 * Delegate that add the 'alex' builder to a project.
 *
 * @author Alejandro Serrano
 */
public class AddNatureActionDelegate extends AbstractAddNatureActionDelegate {

  public AddNatureActionDelegate() {
    // Do nothing
  }

  @Override
  protected String getBuilderID() {
    return AlexBuilder.BUILDER_ID;
  }

}
