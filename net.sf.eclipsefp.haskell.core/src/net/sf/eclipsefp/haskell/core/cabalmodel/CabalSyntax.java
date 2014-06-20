package net.sf.eclipsefp.haskell.core.cabalmodel;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;


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
  SECTION_ELSE("else",true), //$NON-NLS-1$
  SECTION_TESTSUITE("test-suite",true), //$NON-NLS-1$
  SECTION_BENCHMARK("benchmark",true), //$NON-NLS-1$

  FIELD_CATEGORY("category"), //$NON-NLS-1$
  FIELD_EXPOSED_MODULES("exposed-modules"), //$NON-NLS-1$
  FIELD_NAME("name"), //$NON-NLS-1$
  FIELD_LICENSE_FILE("license-file"), //$NON-NLS-1$
  FIELD_LICENSE_FILES("license-files"), //$NON-NLS-1$
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
  FIELD_EXTRA_DOC_FILES("extra-doc-files"),//$NON-NLS-1$
  FIELD_MAIN_IS("main-is"),//$NON-NLS-1$
  FIELD_BUILDABLE("buildable"),//$NON-NLS-1$
  FIELD_OTHER_MODULES("other-modules"),//$NON-NLS-1$
  FIELD_HS_SOURCE_DIRS("hs-source-dirs"),//$NON-NLS-1$
  FIELD_EXTENSIONS("extensions"),//$NON-NLS-1$
  FIELD_DEFAULT_EXTENSIONS("default-extension"),//$NON-NLS-1$
  FIELD_OTHER_EXTENSIONS("other-extensions"),//$NON-NLS-1$
  FIELD_DEFAULT_LANGUAGE("default-language"),//$NON-NLS-1$
  FIELD_OTHER_LANGUAGES("other-languages"),//$NON-NLS-1$
  FIELD_GHC_OPTIONS("ghc-options"),//$NON-NLS-1$
  FIELD_GHC_PROF_OPTIONS("ghc-prof-options"),//$NON-NLS-1$
  FIELD_GHC_SHARED_OPTIONS("ghc-shared-options"),//$NON-NLS-1$
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
  FIELD_TAG("tag"),//$NON-NLS-1$
  FIELD_BUG_REPORTS("bug-reports"),//$NON-NLS-1$
  FIELD_DATA_DIR("data-dir"),//$NON-NLS-1$
  FIELD_BUILD_TOOLS("build-tools"),//$NON-NLS-1$
  FIELD_NHC98_OPTIONS("nhc98-options"),//$NON-NLS-1$
  FIELD_INSTALL_INCLUDES("install-includes"),//$NON-NLS-1$
  FIELD_PKGCONFIG_DEPENDS("pkgconfig-depends"),//$NON-NLS-1$
  FIELD_CPP_OPTIONS("cpp-options"),//$NON-NLS-1$
  FIELD_TEST_MODULE("test-module"),//$NON-NLS-1$

  FIELD_X_USES_TEST_FRAMEWORK("x-uses-tf"),//$NON-NLS-1$
  FIELD_MANUAL("manual"),//$NON-NLS-1$

  VALUE_EXITCODE_STDIO_1_0("exitcode-stdio-1.0",CabalSyntaxType.VALUE),//$NON-NLS-1$
  VALUE_DETAILED_0_9("detailed-0.9",CabalSyntaxType.VALUE),//$NON-NLS-1$

  ;

  public enum CabalSyntaxType {
    FIELD,
    VALUE,
    SECTION
  }

  private final String cabalName;
  private final CabalSyntaxType type;

  private CabalSyntax(final String cabalName){
    this.cabalName=cabalName;
    type=CabalSyntaxType.FIELD;
  }

  private CabalSyntax(final String cabalName,final boolean sectionHeader){
    this.cabalName=cabalName;
    this.type=sectionHeader?CabalSyntaxType.SECTION:CabalSyntaxType.FIELD;
  }

  private CabalSyntax(final String cabalName,final CabalSyntaxType type){
    this.cabalName=cabalName;
    this.type=type;
  }

  public CabalSyntaxType getType() {
    return type;
  }

  public boolean isSectionHeader() {
    return CabalSyntaxType.SECTION.equals(type);
  }

  @Override
  public String toString() {
    return cabalName;
  }

  public String getCabalName() {
    return cabalName;
  }

  public static final Map<String,CabalSyntax> sections=new HashMap<>();
  static {
    for (CabalSyntax cs:CabalSyntax.values() ){
      if (cs.isSectionHeader()){
        sections.put(cs.getCabalName().toLowerCase(Locale.ENGLISH),cs);
      }
    }
  }
}
