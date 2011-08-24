package net.sf.eclipsefp.haskell.core.partitioned.uuagc;

import net.sf.eclipsefp.haskell.core.internal.project.AbstractAddNatureActionDelegate;


public class AddNatureActionDelegate extends AbstractAddNatureActionDelegate {

  public AddNatureActionDelegate() {
    // Do nothing
  }

  @Override
  protected String getBuilderID() {
    return UuagcBuilder.BUILDER_ID;
  }

}
