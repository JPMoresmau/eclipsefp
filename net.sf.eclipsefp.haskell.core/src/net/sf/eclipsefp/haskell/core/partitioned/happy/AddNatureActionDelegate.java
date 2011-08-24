package net.sf.eclipsefp.haskell.core.partitioned.happy;

import net.sf.eclipsefp.haskell.core.internal.project.AbstractAddNatureActionDelegate;


public class AddNatureActionDelegate extends AbstractAddNatureActionDelegate {

  public AddNatureActionDelegate() {
    // Do nothing
  }

  @Override
  protected String getBuilderID() {
    return HappyBuilder.BUILDER_ID;
  }

}
