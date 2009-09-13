package net.sf.eclipsefp.haskell.core.cabalmodel;


/** <p>contains constant definitions for the elements of the Cabal
  * syntax.</p>
  *
  * @author Leif Frenzel
  */
public enum CabalSyntax {
  SECTION_EXECUTABLE("executable",true), //$NON-NLS-1$
  SECTION_LIBRARY("library",true), //$NON-NLS-1$
  SECTION_SOURCE_REPOSITORY("source-repository",true), //$NON-NLS-1$
  SECTION_FLAG("flag",true), //$NON-NLS-1$
  SECTION_IF("if",true), //$NON-NLS-1$

  FIELD_CATEGORY("category"), //$NON-NLS-1$
  FIELD_EXPOSED_MODULES("exposed-modules"), //$NON-NLS-1$
  FIELD_NAME("name"), //$NON-NLS-1$
  FIELD_LICENSE_FILE("license-file"), //$NON-NLS-1$
  FIELD_LICENSE("license"), //$NON-NLS-1$
  FIELD_COPYRIGHT("copyright"), //$NON-NLS-1$
  FIELD_VERSION("version"), //$NON-NLS-1$
  FIELD_AUTHOR("author"), //$NON-NLS-1$
  FIELD_MAINTAINER("maintainer"),//$NON-NLS-1$
  FIELD_SYNOPSIS("synopsis"),//$NON-NLS-1$
  FIELD_DESCRIPTION("description"),//$NON-NLS-1$
  FIELD_HOMEPAGE("homepage"),//$NON-NLS-1$

  FIELD_BUILD_TYPE("build-type"),//$NON-NLS-1$
  FIELD_CABAL_VERSION("cabal-version"),//$NON-NLS-1$
  FIELD_STABILITY("stability"),//$NON-NLS-1$
  FIELD_PACKAGE_URL("package-url"),//$NON-NLS-1$
  FIELD_TESTED_WITH("tested-with"),//$NON-NLS-1$
  FIELD_BUILD_DEPENDS("build-depends"),//$NON-NLS-1$
  FIELD_DATA_FILES("data-files"),//$NON-NLS-1$
  FIELD_EXTRA_SOURCE_FILES("extra-source-files"),//$NON-NLS-1$
  FIELD_EXTRA_TMP_FILES("extra-tmp-files"),//$NON-NLS-1$
  FIELD_MAIN_IS("main-is"),//$NON-NLS-1$
  FIELD_BUILDABLE("buildable"),//$NON-NLS-1$
  FIELD_OTHER_MODULES("other-modules"),//$NON-NLS-1$
  FIELD_HS_SOURCE_DIRS("hs-source-dirs"),//$NON-NLS-1$
  FIELD_EXTENSIONS("extensions"),//$NON-NLS-1$
  FIELD_GHC_OPTIONS("ghc-options"),//$NON-NLS-1$
  FIELD_GHC_PROF_OPTIONS("ghc-prof-options"),//$NON-NLS-1$
  FIELD_HUGS_OPTIONS("hugs-options"),//$NON-NLS-1$
  FIELD_NHC_OPTIONS("nhc-options"),//$NON-NLS-1$
  FIELD_INCLUDES("includes"),//$NON-NLS-1$
  FIELD_INCLUDE_DIRS("include-dirs"),//$NON-NLS-1$
  FIELD_C_SOURCES("c-sources"),//$NON-NLS-1$
  FIELD_EXTRA_LIBRARIES("extra-libraries"),//$NON-NLS-1$
  FIELD_EXTRA_LIB_DIRS("extra-lib-dirs"),//$NON-NLS-1$
  FIELD_CC_OPTIONS("cc-options"),//$NON-NLS-1$
  FIELD_LD_OPTIONS("ld-options"),//$NON-NLS-1$
  FIELD_FRAMEWORKS("frameworks"),//$NON-NLS-1$
  FIELD_DEFAULT("default"),//$NON-NLS-1$
  FIELD_TYPE("type"),//$NON-NLS-1$
  FIELD_LOCATION("location"),//$NON-NLS-1$
  FIELD_TAG("tag")//$NON-NLS-1$

  ;


  private final String cabalName;
  private final boolean sectionHeader;

  private CabalSyntax(final String cabalName){
    this.cabalName=cabalName;
    this.sectionHeader=false;
  }

  private CabalSyntax(final String cabalName,final boolean sectionHeader){
    this.cabalName=cabalName;
    this.sectionHeader=sectionHeader;
  }


  public boolean isSectionHeader() {
    return sectionHeader;
  }

  @Override
  public String toString() {
    return cabalName;
  }

  public String getCabalName() {
    return cabalName;
  }

}
