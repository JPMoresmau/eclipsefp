package net.sf.eclipsefp.haskell.scion.internal.commands;

import org.eclipse.core.resources.IFile;

public class ClassTypeNameCompletions extends CompletionTupleBase {
  public ClassTypeNameCompletions(final IFile theFile) {
    super(theFile);
  }

  @Override
  public String getMethod() {
    return "completion-classTypeNames";
  }
}
