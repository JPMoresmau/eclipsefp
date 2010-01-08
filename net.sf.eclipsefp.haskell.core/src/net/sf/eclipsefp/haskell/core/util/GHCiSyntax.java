package net.sf.eclipsefp.haskell.core.util;

import java.util.regex.Pattern;

/**
 * GHCi syntax strings, for parsing answers
 * @author JP Moresmau
 *
 */
public class GHCiSyntax {

  public static final Pattern BREAKPOINT_SET_PATTERN=Pattern.compile( "Breakpoint (\\d+) activated at (.+)$",Pattern.MULTILINE); //$NON-NLS-1$
  public static final Pattern BREAKPOINT_STOP_PATTERN=Pattern.compile( "Stopped at (.+)$",Pattern.MULTILINE); //$NON-NLS-1$
  public static final Pattern BREAKPOINT_LOCATION_PATTERN=Pattern.compile( "(.+)\\:(\\d+)\\:(\\d+)\\-(\\d+)"); //$NON-NLS-1$
  public static final Pattern BREAKPOINT_LOCATIONMULTILINE_PATTERN=Pattern.compile( "(.+)\\:\\((\\d+),(\\d+)\\)\\-\\((\\d+),(\\d+)\\)");//$NON-NLS-1$
  public static final Pattern BINDING_PATTERN=Pattern.compile("(.+) \\:\\: ([^\\=]+)( \\= (.+))?"); //$NON-NLS-1$

  public static final Pattern BREAKPOINT_NOT=Pattern.compile("not stopped at a breakpoint$",Pattern.MULTILINE); //$NON-NLS-1$

  public static final String UNRESOLVED="_";  //$NON-NLS-1$

  public static final String CONTINUE_COMMAND=":continue"; //$NON-NLS-1$
  public static final String QUIT_COMMAND=":q"; //$NON-NLS-1$
  public static final String DELETE_ALL_BREAKPOINTS_COMMAND=":delete *"; //$NON-NLS-1$
  public static final String SHOW_BINDINGS_COMMAND=":show bindings"; //$NON-NLS-1$
  public static final String STEP_COMMAND=":step"; //$NON-NLS-1$

  public static final String SET_PRINT_WITH_SHOW_COMMAND=":set -fprint-evld-with-show"; //$NON-NLS-1$

  public static final String PROMPT_END="> "; //$NON-NLS-1$

  public static final String TYPEOF="::"; //$NON-NLS-1$

  public static String setBreakpointCommand(final String module, final int lineNumber){
   return  ":break " +module+" "+ lineNumber;  //$NON-NLS-1$//$NON-NLS-2$
  }

  public static String deleteBreakpointCommand(final int breakpointNumber){
    return  ":delete " + breakpointNumber;  //$NON-NLS-1$
  }

  public static String forceVariableCommand(final String name){
    return  ":force " + name;  //$NON-NLS-1$
  }
}
