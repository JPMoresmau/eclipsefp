package net.sf.eclipsefp.haskell.core.hlint;

import net.sf.eclipsefp.haskell.core.internal.project.AbstractAddNatureActionDelegate;


public class AddNatureActionDelegate extends AbstractAddNatureActionDelegate {

  public AddNatureActionDelegate() {
    // Do nothing
  }

  @Override
  protected String getBuilderID() {
    return HLintBuilder.BUILDER_ID;
  }

}
