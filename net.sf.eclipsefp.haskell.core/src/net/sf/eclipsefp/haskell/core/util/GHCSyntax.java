// Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.core.util;

import net.sf.eclipsefp.haskell.util.FileUtil;


/** <p>provides public constants for the command line syntax of ghc and ghc-pkg,
  * GHCs package management tool.</p>
  *
  * <p>This is in core (instead of ghccompiler) because it is also used from
  * the Project Explorer in ui.</p>
  *
  * @author Leif Frenzel
  */
public class GHCSyntax {

  /** <p>The name of the <code>ghc</code> executable.</p>
    *
    * <p>Value is <code>ghc</code>, with the <code>.exe</code> extension
    * appended on Windows systems.</p>
    */
  public static final String GHC     = FileUtil.makeExecutableName( "ghc" ); //$NON-NLS-1$
  /** <p>The name of the <code>ghc-pkg</code> executable.</p>
    *
    * <p>Value is <code>ghc-pkg</code>, with the <code>.exe</code> extension
    * appended on Windows systems.</p>
    */
  public static final String GHC_PKG = FileUtil.makeExecutableName( "ghc-pkg" ); //$NON-NLS-1$
  /** <p>The name of the <code>runghc</code> executable.</p>
    *
    * <p>Value is <code>runghc</code>, with the <code>.exe</code> extension
    * appended on Windows systems.</p>
    */
  public static final String RUN_GHC = FileUtil.makeExecutableName( "runghc" ); //$NON-NLS-1$

  /** <p>The name of the <code>package.conf</code> file.</p> */
  public static final String FILE_PACKAGE_CONF = "package.conf"; //$NON-NLS-1$

  /** <p>The environment variable <code>GHC_PACKAGE_PATH</code>.</p> */
  public static final String ENV_GHC_PACKAGE_PATH = "GHC_PACKAGE_PATH";  //$NON-NLS-1$

  /** <p>The command <code>list</code> (for <code>ghc-pkg</code>).</p> */
  public static final String CMD_LIST = "list"; //$NON-NLS-1$

  /** <p>The <code>--version</code> option.</p> */
  public static final String OPT_VERSION = "--version"; //$NON-NLS-1$
  /** <p>The <code>--numeric-version</code> option.</p> */
  public static final String OPT_NUMERIC_VERSION = "--numeric-version"; //$NON-NLS-1$
  /** <p>The <code>--print-libdir</code> option.</p> */
  public static final String OPT_PRINT_LIBDIR = "--print-libdir"; //$NON-NLS-1$
  /** <p>The <code>+RTS</code> option.</p> */
  public static final String OPT_START_RTS = "+RTS"; //$NON-NLS-1$
  /** <p>The <code>-RTS</code> option.</p> */
  public static final String OPT_END_RTS   = "-RTS"; //$NON-NLS-1$
  /** <p>The <code>--RTS</code> option.</p> */
  public static final String OPT_NO_RTS    = "--RTS"; //$NON-NLS-1$

  public static final String EXTENSIONS="--supported-extensions"; //$NON-NLS-1$
}
