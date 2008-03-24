// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.internal.project;

/** <p>contains common names used in the <code>.hsProject</code> descriptor
  * file (like tag names, attribute names etc.).</p>
  *
  * @author Leif Frenzel
  */
interface IXMLNames {

  public static final String COMPILER_ELEMENT = "compiler"; //$NON-NLS-1$
  /** <p>the single root element of the XML hierarchy in
    * <code>.hsproject</code> files.</p> */
  String DOCUMENT_ELEMENT = "haskellProject"; //$NON-NLS-1$
  /** <p>the element in the <code>.hsproject</code> file that names the source
    * path of the project.</p> */
  String SOURCE_PATH_ELEMENT = "sourcePath"; //$NON-NLS-1$
  /** <p>the element in the <code>.hsproject</code> file that names the output
    * path of the project.</p> */
  String OUTPUT_PATH_ELEMENT = "outputPath"; //$NON-NLS-1$
  /** <p>the element in the <code>.hsproject</code> file that names the target
    * binary name of the project.</p> */
  String TARGET_NAME_ELEMENT = "targetBinary"; //$NON-NLS-1$

  /** <p>The 'name' attribute.</p> */
  String NAME_ATT = "name"; //$NON-NLS-1$
  /** <p>The 'path' attribute.</p> */
  String PATH_ATT = "path"; //$NON-NLS-1$
}