// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

/** <p>contains constant defintions for command line paramters of GHC.</p>
  * 
  * <p>Section references are to sections in the GHC manual.</p>
  * 
  * @author Leif Frenzel
  */
public interface IGhcParameters {
  
  String VERSION = "--version";
  String PRINT_LIBDIR = "--print-libdir"; 

  // Language options (Section 7.1)
  String LANG_GLASGOW_EXTS                = "-fglasgow-exts";
  String LANG_FI                          = "-ffi";
  String LANG_FFI                         = "-fffi";
  String LANG_WITH                        = "-fwith";
  String LANG_NO_MONOMORPHISM_RESTRICTION = "-fno-monomorphism-restriction";
  String LANG_ALLOW_OVERLAPPING_INSTANCES = "-fallow-overlapping-instances"; 
  String LANG_ALLOW_UNDECIDABLE_INSTANCES = "-fallow-undecidable-instances"; 
  String LANG_ALLOW_INCOHERENT_INSTANCES  = "-fallow-incoherent-instances";
  String LANG_CONTEXT_STACK               = "-fcontext-stack";
  String LANG_INLINE_PHRASE               = "-finline-phase";
  String LANG_GENERICS                    = "-fgenerics";
  String LANG_NO_IMPLICIT_PRELUDE         = "-fno-implicit-prelude";
  
  // General Optimisation (Section 4.11.1)
  String OPT_O0 = "-O0";
  String OPT_O1 = "-O1";
  String OPT_O2 = "-O2";
  
  // Individual optimisations (Section 4.11.2)
  String OPT_CASE_MERGE                   = "-fcase-merge";
  String OPT_DICTS_STRICT                 = "-fdicts-strict";
  String OPT_DO_ETA_REDUCTION             = "-fdo-eta-reduction";
  String OPT_DO_LAMBDA_ETA_EXPANSION      = "-fdo-lambda-eta-expansion";
  String OPT_EXCESS_PRECISION             = "-fexcess-precision";
  String OPT_FOLDR_BUILD_ON               = "-ffoldr-build-on";
  String OPT_IGNORE_ASSERTS               = "-fignore-asserts";
  String OPT_IGNORE_INTERFACE_PRAGMAS     = "-fignore-interface-pragmas";
  String OPT_LET_NO_ESCAPE                = "-flet-no-escape";
  String OPT_LIBERATE_CASE_THRESHOLD      = "-fliberate-case-threshold";
  String OPT_OMIT_INTERFACE_PRAGMAS       = "-fomit-interface-pragmas";
  String OPT_MAX_WORKER_ARGS              = "-fmax-worker-args";
  String OPT_MAX_SIMPLIFIER_ITERATIONS    = "-fmax-simplifier-iterations";
  String OPT_NO_CPR                       = "-fno-cpr";
  String OPT_NO_CSE                       = "-fno-cse";
  String OPT_NO_PRE_INLINING              = "-fno-pre-inlining";
  String OPT_NO_STRICTNESS                = "-fno-strictness";
  String OPT_NUMBERS_STRICT               = "-fnumbers-strict";
  String OPT_UNBOX_STRICT_FIELDS          = "-funbox-strict-fields";
  String OPT_UNFOLDING_CREATION_THRESHOLD = "-funfolding-creation-threshold";
  String OPT_UNFOLDING_FUN_DISCOUNT       = "-funfolding-fun-discount";
  String OPT_UNFOLDING_KEENESS_FACTOR     = "-funfolding-keeness-factor";
  String OPT_UNFOLDING_UPDATE_IN_PLACE    = "-funfolding-update-in-place";
  String OPT_UNFOLDING_USE_THRESHOLD      = "-funfolding-use-threshold";
  String OPT_USAGESP                      = "-fusagesp";
}