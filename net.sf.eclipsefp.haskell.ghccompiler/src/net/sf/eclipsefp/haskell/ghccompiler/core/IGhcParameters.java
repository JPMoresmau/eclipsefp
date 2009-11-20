// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.core;

/** <p>contains constant defintions for command line paramters of GHC.</p>
  *
  * <p>Section references are to sections in the GHC manual.</p>
  *
  * @author Leif Frenzel
  * @deprecated, use GhcParameter instead
  */
public interface IGhcParameters {

  String VERSION         = "--version"; //$NON-NLS-1$
  String NUMERIC_VERSION = "--numeric-version"; //$NON-NLS-1$
  String PRINT_LIBDIR    = "--print-libdir"; //$NON-NLS-1$

  // Language options (Section 7.1)
  String LANG_GLASGOW_EXTS                = "-fglasgow-exts"; //$NON-NLS-1$
  String LANG_FI                          = "-ffi"; //$NON-NLS-1$
  String LANG_FFI                         = "-fffi"; //$NON-NLS-1$
  String LANG_WITH                        = "-fwith"; //$NON-NLS-1$
  String LANG_NO_MONOMORPHISM_RESTRICTION = "-fno-monomorphism-restriction"; //$NON-NLS-1$
  String LANG_ALLOW_OVERLAPPING_INSTANCES = "-fallow-overlapping-instances"; //$NON-NLS-1$
  String LANG_ALLOW_UNDECIDABLE_INSTANCES = "-fallow-undecidable-instances"; //$NON-NLS-1$
  String LANG_ALLOW_INCOHERENT_INSTANCES  = "-fallow-incoherent-instances"; //$NON-NLS-1$
  String LANG_CONTEXT_STACK               = "-fcontext-stack"; //$NON-NLS-1$
  String LANG_INLINE_PHRASE               = "-finline-phase"; //$NON-NLS-1$
  String LANG_GENERICS                    = "-fgenerics"; //$NON-NLS-1$
  String LANG_NO_IMPLICIT_PRELUDE         = "-fno-implicit-prelude"; //$NON-NLS-1$

  // General Optimisation (Section 4.11.1)
  String OPT_O0 = "-O0"; //$NON-NLS-1$
  String OPT_O1 = "-O1"; //$NON-NLS-1$
  String OPT_O2 = "-O2"; //$NON-NLS-1$

  // Individual optimisations (Section 4.11.2)
  String OPT_CASE_MERGE                   = "-fcase-merge"; //$NON-NLS-1$
  String OPT_DICTS_STRICT                 = "-fdicts-strict"; //$NON-NLS-1$
  String OPT_DO_ETA_REDUCTION             = "-fdo-eta-reduction"; //$NON-NLS-1$
  String OPT_DO_LAMBDA_ETA_EXPANSION      = "-fdo-lambda-eta-expansion"; //$NON-NLS-1$
  String OPT_EXCESS_PRECISION             = "-fexcess-precision"; //$NON-NLS-1$
  String OPT_FOLDR_BUILD_ON               = "-ffoldr-build-on"; //$NON-NLS-1$
  String OPT_IGNORE_ASSERTS               = "-fignore-asserts"; //$NON-NLS-1$
  String OPT_IGNORE_INTERFACE_PRAGMAS     = "-fignore-interface-pragmas"; //$NON-NLS-1$
  String OPT_LET_NO_ESCAPE                = "-flet-no-escape"; //$NON-NLS-1$
  String OPT_LIBERATE_CASE_THRESHOLD      = "-fliberate-case-threshold"; //$NON-NLS-1$
  String OPT_OMIT_INTERFACE_PRAGMAS       = "-fomit-interface-pragmas"; //$NON-NLS-1$
  String OPT_MAX_WORKER_ARGS              = "-fmax-worker-args"; //$NON-NLS-1$
  String OPT_MAX_SIMPLIFIER_ITERATIONS    = "-fmax-simplifier-iterations"; //$NON-NLS-1$
  String OPT_NO_CPR                       = "-fno-cpr"; //$NON-NLS-1$
  String OPT_NO_CSE                       = "-fno-cse"; //$NON-NLS-1$
  String OPT_NO_PRE_INLINING              = "-fno-pre-inlining"; //$NON-NLS-1$
  String OPT_NO_STRICTNESS                = "-fno-strictness"; //$NON-NLS-1$
  String OPT_NUMBERS_STRICT               = "-fnumbers-strict"; //$NON-NLS-1$
  String OPT_UNBOX_STRICT_FIELDS          = "-funbox-strict-fields"; //$NON-NLS-1$
  String OPT_UNFOLDING_CREATION_THRESHOLD = "-funfolding-creation-threshold"; //$NON-NLS-1$
  String OPT_UNFOLDING_FUN_DISCOUNT       = "-funfolding-fun-discount"; //$NON-NLS-1$
  String OPT_UNFOLDING_KEENESS_FACTOR     = "-funfolding-keeness-factor"; //$NON-NLS-1$
  String OPT_UNFOLDING_UPDATE_IN_PLACE    = "-funfolding-update-in-place"; //$NON-NLS-1$
  String OPT_UNFOLDING_USE_THRESHOLD      = "-funfolding-use-threshold"; //$NON-NLS-1$
  String OPT_USAGESP                      = "-fusagesp"; //$NON-NLS-1$
}