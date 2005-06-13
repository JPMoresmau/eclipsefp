// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

/** <p>defines constant names for declaration types.</p>
  *
  * @author Leif Frenzel
  */
interface IElementTypes {

  // declaration types
  int TYPE_DECL        = 0;
  int DATA_DECL        = 1;
  int INFIX_DECL       = 2;
  int NEWTYPE_DECL     = 3;
  int CLASS_DECL       = 4;
  int INSTANCE_DECL    = 5;
  int DEFAULT_DECL     = 6;
  int TYPE_SIGNATURE   = 7;
  int FUNCTION_BINDING = 8;
  int PATTERN_BINDING  = 9;
  // export specification types
  int EXPORT_VARIABLE       = 10;
  int EXPORT_ABSOLUTE       = 11;
  int EXPORT_THING_ALL      = 12;
  int EXPORT_THING_WITH     = 13;
  int EXPORT_MODULE_CONTENT = 14;
  // import specification types
  int IMPORT_VARIABLE       = 15;
  int IMPORT_ABSOLUTE       = 16;
  int IMPORT_THING_ALL      = 17;
  int IMPORT_THING_WITH     = 18;
}
