package net.sf.eclipsefp.haskell.core.hlint;

import net.sf.eclipsefp.haskell.core.internal.project.AbstractAddNatureActionDelegate;

/**
 * Delegate that add the 'hlint' builder to a project.
 *
 * @author Alejandro Serrano
 */
public class AddNatureActionDelegate extends AbstractAddNatureActionDelegate {

  public AddNatureActionDelegate() {
    // Do nothing
  }

  @Override
  protected String getBuilderID() {
    return HLintBuilder.BUILDER_ID;
  }

}
