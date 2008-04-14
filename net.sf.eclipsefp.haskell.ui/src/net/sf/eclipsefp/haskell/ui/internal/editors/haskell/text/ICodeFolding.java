package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import java.util.List;

public interface ICodeFolding {

  public interface ICodeFoldingRegion {
    int getStartLine();
    int getEndLine();
  }

  List<ICodeFoldingRegion> performCodeFolding( String param );
}
