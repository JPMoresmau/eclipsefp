package de.leiffrenzel.fp.haskell.core.code;

public enum EHaskellCommentStyle {
  
  USUAL ("hs") , 
  LITERATE ("lhs"),
  TEX ("lhs");
  
  private String fFileExtension;

  private EHaskellCommentStyle(final String fileExtension) {
    fFileExtension = fileExtension;
  }

  public String getFileExtension() {
    return fFileExtension;
  }

}
