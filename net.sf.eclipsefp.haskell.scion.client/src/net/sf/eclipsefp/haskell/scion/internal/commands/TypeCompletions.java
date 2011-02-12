package net.sf.eclipsefp.haskell.scion.internal.commands;

import org.eclipse.core.resources.IFile;

public class TypeCompletions extends CompletionTupleBase {
  public TypeCompletions(final IFile theFile) {
    super(theFile);
  }

  public String getMethod() {
    return "completion-types";
  }
}
