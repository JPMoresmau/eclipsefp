package net.sf.eclipsefp.haskell.core.code;

import net.sf.eclipsefp.haskell.util.FileUtil;

public enum EHaskellCommentStyle {

  USUAL( FileUtil.EXTENSION_HS ),
  LITERATE( FileUtil.EXTENSION_LHS ),
  TEX( FileUtil.EXTENSION_LHS );

  private String fFileExtension;

  private EHaskellCommentStyle( final String fileExtension ) {
    fFileExtension = fileExtension;
  }

  public String getFileExtension() {
    return fFileExtension;
  }

}
