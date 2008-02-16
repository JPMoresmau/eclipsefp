package net.sf.eclipsefp.haskell.core.code;

public enum EHaskellCommentStyle {

  USUAL( "hs" ), LITERATE( "lhs" ), TEX( "lhs" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

  private String fFileExtension;

  private EHaskellCommentStyle( final String fileExtension ) {
    fFileExtension = fileExtension;
  }

  public String getFileExtension() {
    return fFileExtension;
  }

}
