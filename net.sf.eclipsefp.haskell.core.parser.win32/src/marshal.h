// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
//
// marshal.h - declares the marshalling functions (implemented in Haskell)
#ifndef MARSHAL_H
#define MARSHAL_H

  // modules

  // we assume there is one module per compilation unit (which is how the
  // parser handles things). This returns the name of the module for the 
  // pointer to the parse result
  extern char* mrsh_moduleName( HsStablePtr parseResult );
  // the line of the source location for the module
  extern int mrsh_moduleLine( HsStablePtr parseResult );
  // the column of the source location for the module
  extern int mrsh_moduleColumn( HsStablePtr parseResult );

  // export specifications
  
  // the number of export specifications for the module in the parse result
  extern int mrsh_exportCount( HsStablePtr parseResult );
  // type code for the constructor of the export specification at the 
  // specified position
  extern int mrsh_exportSpecificationType( HsStablePtr parseResult, jint pos );
  // name of the export specification at the specified position (the 
  // identifier of the exported element)
  extern char* mrsh_exportSpecificationName( HsStablePtr parseResult, 
                                             jint pos );
  // the number of exported names in the export specification at the specified
  // position in the module
  extern int mrsh_exportThingWithCount( HsStablePtr parseResult, jint pos );
  // name of the exported element at position pos in the list of the export
  // specification list, assuming that the export specification is a
  // thing-with declaration
  extern char* mrsh_exportThingWithName( HsStablePtr parseResult, jint, jint );

  // imports
  
  // the number of imports for the module in the parse result
  extern int mrsh_importCount( HsStablePtr parseResult );
  // the imported module at the specified position
  extern char* mrsh_importedElement( HsStablePtr parseResult, jint pos );
  // the line of the source location for the import at the specified position
  extern int mrsh_importLine( HsStablePtr parseResult, jint pos );
  // the column of the source location for the import at the specified position
  extern int mrsh_importColumn( HsStablePtr parseResult, jint pos );

  // returns the count of import specifications for the import at the specified
  // position in the imports list of the module in the parse result
  extern int mrsh_importSpecificationCount( HsStablePtr, jint );
  // whether the import at the specified position in the imports list is
  // declared as 'hiding'
  extern jboolean mrsh_importSpecificationHiding( HsStablePtr, jint );
  // type code for the constructor of the import specification at the 
  // specified position in the list of import specifications for the import
  // at the specified position in the imports list of the module
  extern int mrsh_importSpecificationType( HsStablePtr, jint, jint );
  // returns the name of the import specification at the specified position
  // in the import specification list of the import at the specified position
  // in the import list of the module in the parse result                                                
  extern char* mrsh_importSpecificationName( HsStablePtr, jint, jint );
  // gets from the module in the parse result the import, the import spec 
  // (which is assumed to be thing-with) and returns from that count of 
  // components
  extern int mrsh_importThingWithCount( HsStablePtr, jint, jint );
  // returns from the module in the parse result the import, the import spec 
  // (which is assumed to be thing-with) and from that the component, at
  // the specified positions, resp.
  extern char* mrsh_importThingWithName( HsStablePtr, jint, jint, jint );

  // declarations

  // the number of declarations for the module in the parse result
  extern int mrsh_declCount( HsStablePtr );
  // type code for the constructor of the decl at the specified position
  extern int mrsh_declType( HsStablePtr, jint );
  // name of the decl at the specified position
  extern char* mrsh_declName( HsStablePtr, jint );
  // the line of the source location for the declaration at the 
  // specified position
  extern int mrsh_declLine( HsStablePtr, jint );
  // the column of the source location for the declaration at the 
  // specified position
  extern int mrsh_declColumn( HsStablePtr, jint );
  // assuming the declaration at the specified position is a type signature,
  // return how many identifiers are declared in it
  extern int mrsh_typeSigIdentifierCount( HsStablePtr, jint );
  // assuming the declaration at the specified position declPos is a type
  // signature, return the name of the identifier at position pos in the 
  // list of identifiers for which the type is declared
  extern char* mrsh_typeSigIdentifier( HsStablePtr, jint, jint );
  // returns the count of members in the class declaration at the 
  // specified position in the declarations list
  extern int mrsh_classDeclMemberCount( HsStablePtr parseResult, 
                                        jint declPos );
  // whether the member at the specified position in the members list of the
  // declaration at declPos is a type signature
  extern jboolean mrsh_typeSignMember( HsStablePtr, jint, jint );
  // returns the line number in the source file of the type signature in the 
  // class declaration at the specified position in the declarations list
  extern int mrsh_classDeclTypeSigLine( HsStablePtr parseResult, 
                                        jint declPos, 
                                        jint typeSigPos );
  // returns the column in the source file of the type signature in the 
  // class declaration at the specified position in the declarations list
  extern int mrsh_classDeclTypeSigColumn( HsStablePtr parseResult, 
                                          jint declPos, 
                                          jint typeSigPos );
  // returns the number of identifiers of the type signature in the 
  // class declaration at the specified position in the declarations list
  extern int mrsh_classDeclTypeSigIdentifierCount( HsStablePtr parseResult, 
                                                   jint declPos, 
                                                   jint typeSigPos );
  // returns the identifier at the position pos in the type signature in the 
  // class declaration at position declPos in the declarations list
  // getClassDeclTypeSigIdentifierName( jint, jint, jint, jint );
  extern char* mrsh_classDeclTypeSigIdentifierName( HsStablePtr parseResult, 
                                                    jint declPos, 
                                                    jint typeSigPos, 
                                                    jint identifierPos );
  // returns the precedence level of the infix declaration at the position
  // declPos in the declarations list
  extern int mrsh_infixDeclPrecedenceLevel( HsStablePtr parseResult, 
                                            jint declPos );
  // returns the associativity of the infix declaration at the position
  // declPos in the declarations list
  extern int mrsh_infixDeclAssociativity( HsStablePtr parseResult, 
                                          jint declPos );
  // returns the count of operators in the infix declaration at the position
  // declPos in the declarations list
  extern int mrsh_infixDeclOperatorCount( HsStablePtr parseResult, 
                                          jint declPos );
  // returns the name of the operator at position pos in the operators list 
  // in the infix declaration at the position declPos in the declarations list
  extern char* mrsh_infixDeclOperator( HsStablePtr parseResult, 
                                       jint declPos, 
                                       jint pos );
  // returns the count of constructors in the data declaration at position
  // declPos in the declarations list
  extern int mrsh_dataDeclConstructorCount( HsStablePtr parseResult, 
                                            jint declPos );
  // returns the name of the constructor at position constPos in the data 
  // declaration at position declPos in the declarations list
  extern char* mrsh_dataDeclConstructorName( HsStablePtr parseResult, 
                                             jint declPos, 
                                             jint constPos );
  // returns the line in the source file of the constructor at position 
  // constPos in the data declaration at position declPos in the declarations 
  // list
  extern int mrsh_dataDeclConstructorLine( HsStablePtr parseResult, 
                                           jint declPos, 
                                           jint constPos );
  // returns the column in the source file of the constructor at position 
  // constPos in the data declaration at position declPos in the declarations 
  // list
  extern int mrsh_dataDeclConstructorColumn( HsStablePtr parseResult, 
                                             jint declPos, 
                                             jint constPos );
  // returns the count of matches in the function binding at position
  // declPos in the declarations list
  extern int mrsh_functionBindingMatchCount( HsStablePtr parseResult, 
                                             jint declPos );
  // returns the name of the match at position  matchPos in the function 
  // binding at position declPos in the declarations list
  extern char* mrsh_functionBindingMatchName( HsStablePtr parseResult, 
                                              jint declPos, 
                                              jint matchPos );
  // returns the line in the source file of the match at position 
  // matchPos in the function binding at position declPos in the declarations 
  // list
  extern int mrsh_functionBindingMatchLine( HsStablePtr parseResult, 
                                            jint declPos, 
                                            jint matchPos );
  // returns the column in the source file of the match at position 
  // matchPos in the function binding at position declPos in the declarations 
  // list
  extern int mrsh_functionBindingMatchColumn( HsStablePtr parseResult, 
                                              jint declPos, 
                                              jint matchPos );

#endif  // MARSHAL_H
