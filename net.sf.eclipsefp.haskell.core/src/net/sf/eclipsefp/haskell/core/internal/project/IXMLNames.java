// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

/** <p>contains common names used in the <code>.hsProject</code> descriptor
  * file (like tag names, attribute names etc.).</p>
  *
  * @author Leif Frenzel
  */
interface IXMLNames {

  String COMPILER_ELEMENT = "compiler"; //$NON-NLS-1$
  /** <p>the single root element of the XML hierarchy in
    * <code>.hsproject</code> files.</p> */
  String DOCUMENT_ELEMENT = "haskellProject"; //$NON-NLS-1$
  /** <p>the element in the <code>.hsproject</code> file that names the
    * simple project information node.</p> */
  String SIMPLE_PROJECT_ELEMENT = "simpleProject"; //$NON-NLS-1$
  String CABAL_PROJECT_ELEMENT = "cabalProject"; //$NON-NLS-1$
  /** <p>the element in the <code>.hsproject</code> file that names the source
    * paths of the project.</p> */
  String SOURCE_PATHS_ELEMENT = "sourcePaths"; //$NON-NLS-1$
  /** <p>the element in the <code>.hsproject</code> file that names the output
    * path of the project.</p> */
  String OUTPUT_PATH_ELEMENT = "outputPath"; //$NON-NLS-1$
  /** <p>the element in the <code>.hsproject</code> file that names the targets
    * of the project.</p> */
  String BIN_PATH_ELEMENT = "binPath"; //$NON-NLS-1$
  String TARGETS_ELEMENT = "targets"; //$NON-NLS-1$
  String PATH_ELEMENT = "path"; //$NON-NLS-1$
  String EXECUTABLE_TARGET_ELEMENT = "executableTarget"; //$NON-NLS-1$
  String MAIN_ELEMENT = "main"; //$NON-NLS-1$
}