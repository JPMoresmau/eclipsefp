package net.sf.eclipsefp.haskell.buildwrapper.types;

/**
 * <p>GHC messages strings used for quick fixes and assist.
 * This may need to be localized if/when GHc is localized
 * </p>
  *
  * @author JP Moresmau
 */
public interface GhcMessages {

  public static String WARNING_NOTYPE_CONTAINS="definition but no type signature"; //$NON-NLS-1$

  public static String WARNING_NOTYPE_TOPLEVEL_CONTAINS="top-level binding with no type signature:"; //$NON-NLS-1$
	
  public static String WARNING_USEFLAG_CONTAINS="you intended to use"; //$NON-NLS-1$
  public static String WARNING_USEFLAG_CONTAINS2="perhaps you intended"; //$NON-NLS-1$
  public static String WARNING_USEFLAG_CONTAINS3="use -x"; //$NON-NLS-1$
  public static String WARNING_SUPPRESS_CONTAINS="to suppress this message"; //$NON-NLS-1$

  
  public static String WARNING_INFERREDTYPE_START="inferred type:"; //$NON-NLS-1$

  public static String WARNING_IMPORT_USELESS_CONTAINS="is imported, but nothing from it is used";//$NON-NLS-1$
  public static String WARNING_IMPORT_USELESS_START="to suppress this warning, use:";//$NON-NLS-1$
  
  public static String WARNING_IMPORT_USELESS_CONTAINS2="is redundant";//$NON-NLS-1$
  public static String WARNING_IMPORT_USELESS_START2="to import instances alone, use:";//$NON-NLS-1$
  public static String WARNING_IMPORT_USELESS_ELEMENT2="from module";//$NON-NLS-1$
  
  public static String ERROR_INTERACTIVE_DISABLED="interactive check disabled"; //$NON-NLS-1$

  public static String MISSING_MODULE="could not find module";//$NON-NLS-1$
  public static String MISSING_MODULE_ADD_START="perhaps you need to add `";//$NON-NLS-1$	  
  public static String MISSING_MODULE_ADD_END="' to the build-depends"; //$NON-NLS-1$
  
  public static String NOT_IN_SCOPE_START="not in scope: "; //$NON-NLS-1$
  public static String NOT_IN_SCOPE_END="'"; //$NON-NLS-1$
  public static String NOT_IN_SCOPE_SUGGESTION="perhaps you meant"; //$NON-NLS-1$
  public static String NOT_IN_SCOPE_SUGGESTION_MULTIPLE="perhaps you meant one of these:"; //$NON-NLS-1$
  
  public static String IS_A_DATA_CONSTRUCTOR="is a data constructor"; //$NON-NLS-1$
  
  public static String DO_DISCARDED_START="a do-notation statement discarded a result";
  public static String DO_DISCARDED_FIX="by saying \"_ <- ";
  
  public static String NOT_ENABLED=" is not enabled";
  public static String PERMITS_THIS=" permits this";
  public static String YOU_NEED="you need -x";
  public static String TRY="try -x";
  
  public static String CAST_FROM_CHAR="with actual type `[char]'";

  //-fno-warn-unused-do-bind


}
