// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.core.project;

/** <p>contains common names used in the <code>.hsProject</code> descriptor
  * file (like tag names, attribute names etc.).</p>
  * 
  * @author Leif Frenzel
  */
interface IXMLNames {

  /** <p>the single root element of the XML hierarchy in 
    * <code>.hsproject</code> files.</p> */
  String DOCUMENT_ELEMENT = "haskellProject";
  /** <p>the element in the <code>.hsproject</code> file that names the source 
    * path of the project.</p> */
  String SOURCE_PATH_ELEMENT = "sourcePath";
  /** <p>the element in the <code>.hsproject</code> file that names the output 
    * path of the project.</p> */
  String OUTPUT_PATH_ELEMENT = "outputPath";
  /** <p>the element in the <code>.hsproject</code> file that names the 
    * binaries path of the project.</p> */
  String BIN_PATH_ELEMENT = "binPath";
  /** <p>the element in the <code>.hsproject</code> file that names the target 
    * binary name of the project.</p> */
  String TARGET_NAME_ELEMENT = "targetBinary";

  /** <p>The 'name' attribute.</p> */
  String NAME_ATT = "name";
  /** <p>The 'path' attribute.</p> */
  String PATH_ATT = "path";
}