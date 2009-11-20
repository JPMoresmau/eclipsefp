package net.sf.eclipsefp.haskell.ghccompiler.core;


public enum GhcParameter {

  VERSION ("--version",GhcParameterType.GENERAL), //$NON-NLS-1$
  NUMERIC_VERSION ( "--numeric-version",GhcParameterType.GENERAL), //$NON-NLS-1$
  PRINT_LIBDIR   ( "--print-libdir",GhcParameterType.GENERAL), //$NON-NLS-1$
  LANG_GLASGOW_EXTS ("-fglasgow-exts",GhcParameterType.LANGUAGE), //$NON-NLS-1$
  LANG_FI    ("-ffi",GhcParameterType.LANGUAGE,"-XForeignFunctionInterface",Util.r6_8_1), //$NON-NLS-1$ //$NON-NLS-2$
  LANG_FFI                         ( "-fffi",GhcParameterType.LANGUAGE,"-XForeignFunctionInterface",Util.r6_8_1), //$NON-NLS-1$ //$NON-NLS-2$
  LANG_WITH                        ( "-fwith",GhcParameterType.LANGUAGE), //$NON-NLS-1$
  LANG_NO_MONOMORPHISM_RESTRICTION ( "-fno-monomorphism-restriction",GhcParameterType.LANGUAGE,"-XNoMonomorphismRestriction",Util.r6_8_1), //$NON-NLS-1$ //$NON-NLS-2$
  LANG_ALLOW_OVERLAPPING_INSTANCES ( "-fallow-overlapping-instances",GhcParameterType.LANGUAGE,"-XOverlappingInstances",Util.r6_8_1), //$NON-NLS-1$ //$NON-NLS-2$
  LANG_ALLOW_UNDECIDABLE_INSTANCES ( "-fallow-undecidable-instances",GhcParameterType.LANGUAGE,"-XUndecidableInstances",Util.r6_8_1), //$NON-NLS-1$ //$NON-NLS-2$
  LANG_ALLOW_INCOHERENT_INSTANCES  ( "-fallow-incoherent-instances",GhcParameterType.LANGUAGE,"-XIncoherentInstances",Util.r6_8_1), //$NON-NLS-1$ //$NON-NLS-2$
  LANG_GENERICS                    ( "-fgenerics",GhcParameterType.LANGUAGE,"-XGenerics",Util.r6_8_1), //$NON-NLS-1$ //$NON-NLS-2$
  LANG_NO_IMPLICIT_PRELUDE         ( "-fno-implicit-prelude",GhcParameterType.LANGUAGE,"-XNoImplicitPrelude",Util.r6_8_1), //$NON-NLS-1$ //$NON-NLS-2$

  // General Optimisation (Section 4.11.1)
  OPT_O0 ( "-O0",GhcParameterType.OPTIMIZATION_GENERAL), //$NON-NLS-1$
  OPT_O1 ( "-O1",GhcParameterType.OPTIMIZATION_GENERAL), //$NON-NLS-1$
  OPT_O2 ( "-O2",GhcParameterType.OPTIMIZATION_GENERAL), //$NON-NLS-1$

  // Individual optimisations (Section 4.11.2)
  OPT_CASE_MERGE                   ( "-fcase-merge",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_DICTS_STRICT                 ( "-fdicts-strict",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_DO_ETA_REDUCTION             ( "-fdo-eta-reduction",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_DO_LAMBDA_ETA_EXPANSION      ( "-fdo-lambda-eta-expansion",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_EXCESS_PRECISION             ( "-fexcess-precision",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_FOLDR_BUILD_ON               ( "-ffoldr-build-on",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_IGNORE_ASSERTS               ( "-fignore-asserts",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_IGNORE_INTERFACE_PRAGMAS     ( "-fignore-interface-pragmas",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_LET_NO_ESCAPE                ( "-flet-no-escape",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
//  OPT_LIBERATE_CASE_THRESHOLD      ( "-fliberate-case-threshold",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_OMIT_INTERFACE_PRAGMAS       ( "-fomit-interface-pragmas",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
//  OPT_MAX_WORKER_ARGS              ( "-fmax-worker-args",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
//  OPT_MAX_SIMPLIFIER_ITERATIONS    ( "-fmax-simplifier-iterations",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_NO_CPR                       ( "-fno-cpr",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_NO_CSE                       ( "-fno-cse",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_NO_PRE_INLINING              ( "-fno-pre-inlining",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_NO_STRICTNESS                ( "-fno-strictness",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_NUMBERS_STRICT               ( "-fnumbers-strict",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_UNBOX_STRICT_FIELDS          ( "-funbox-strict-fields",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
//  OPT_UNFOLDING_CREATION_THRESHOLD ( "-funfolding-creation-threshold",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
//  OPT_UNFOLDING_FUN_DISCOUNT       ( "-funfolding-fun-discount",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
//  OPT_UNFOLDING_KEENESS_FACTOR     ( "-funfolding-keeness-factor",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
//  OPT_UNFOLDING_UPDATE_IN_PLACE    ( "-funfolding-update-in-place",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
//  OPT_UNFOLDING_USE_THRESHOLD      ( "-funfolding-use-threshold",GhcParameterType.OPTIMIZATION_SPECIFIC), //$NON-NLS-1$
  OPT_USAGESP                      ( "-fusagesp",GhcParameterType.OPTIMIZATION_SPECIFIC); //$NON-NLS-1$


  private String name;
  private String newName;
  private String newVersion;
  private GhcParameterType type;

  private GhcParameter(final String name,final GhcParameterType type){
    this.name=name;
    this.type=type;
  }

  private GhcParameter(final String name,final GhcParameterType type,final String newName,final String newVersion){
    this.name=name;
    this.type=type;
    this.newName=newName;
    this.newVersion=newVersion;
  }


  public String getName() {
    return name;
  }

  public GhcParameterType getType() {
    return type;
  }

  public String getName(final String targetVersion) {
    if (targetVersion!=null && newVersion != null && newName!=null){
      if (Util.compareTargets( targetVersion, newVersion )>=0){
        return newName;
      }
    }
    return name;
  }

}
