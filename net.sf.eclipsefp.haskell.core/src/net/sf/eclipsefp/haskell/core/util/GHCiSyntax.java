package net.sf.eclipsefp.haskell.core.util;

import java.util.regex.Pattern;
import net.sf.eclipsefp.haskell.util.PlatformUtil;

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
  public static final Pattern BINDING_PATTERN=Pattern.compile("(.+)\\s+\\:\\:([^\\=]+)( \\= (.+))?",Pattern.MULTILINE | Pattern.DOTALL); //$NON-NLS-1$
  public static final Pattern CONTEXT_PATTERN=Pattern.compile("--> (.+)$",Pattern.MULTILINE); //$NON-NLS-1$

  public static final Pattern BREAKPOINT_NOT=Pattern.compile("not stopped at a breakpoint$",Pattern.MULTILINE); //$NON-NLS-1$



  public static final String UNRESOLVED="_";  //$NON-NLS-1$

  public static final String CONTINUE_COMMAND=":continue"; //$NON-NLS-1$
  public static final String QUIT_COMMAND=":q"; //$NON-NLS-1$
  public static final String DELETE_ALL_BREAKPOINTS_COMMAND=":delete *"; //$NON-NLS-1$
  public static final String SHOW_BINDINGS_COMMAND=":show bindings"; //$NON-NLS-1$
  public static final String SHOW_CONTEXT_COMMAND=":show context"; //$NON-NLS-1$

  public static final String TYPE_LAST_RESULT_COMMAND=":t it"; //$NON-NLS-1$
  public static final String STEP_COMMAND=":step"; //$NON-NLS-1$



  public static final String SET_PRINT_WITH_SHOW_COMMAND=":set -fprint-evld-with-show"; //$NON-NLS-1$

  public static final String SET_BREAK_ON_ERROR_COMMAND=":set -fbreak-on-error"; //$NON-NLS-1$
  public static final String SET_BREAK_ON_EXCEPTION_COMMAND=":set -fbreak-on-exception"; //$NON-NLS-1$

  public static final String PROMPT_END="> "; //$NON-NLS-1$

  public static final String TYPEOF="::"; //$NON-NLS-1$
  public static final String UNIT="()"; //$NON-NLS-1$

  public static String addModuleCommand(final String module){
    return  ":add *" +module;  //$NON-NLS-1$
   }

  public static String loadModuleCommand(final String module){
    return  ":load *" +module;  //$NON-NLS-1$
   }

  public static String setBreakpointCommand(final String module, final int lineNumber){
   return  ":break " +module+" "+ lineNumber;  //$NON-NLS-1$//$NON-NLS-2$
  }

  public static String deleteBreakpointCommand(final int breakpointNumber){
    return  ":delete " + breakpointNumber;  //$NON-NLS-1$
  }

  public static String forceVariableCommand(final String name){
    return  ":force " + name;  //$NON-NLS-1$
  }

  public static String formatType(final String type){
    String newType=type.replaceAll( PlatformUtil.NL, "" ); //$NON-NLS-1$
    newType=newType.replaceAll("  ",""); //$NON-NLS-1$ //$NON-NLS-2$
    return newType.trim();
  }
}
