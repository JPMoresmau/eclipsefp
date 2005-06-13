-- Copyright (c) 2004-2005 by Leif Frenzel
-- See http://leiffrenzel.de
{- | ParserImpl implements the parsing and marshalling functions which are 
     called (over a bridge in c code) from the Java side. -}
module ParserImpl where

import Foreign
import Language.Haskell.Parser
import Language.Haskell.Syntax
import CString

{- | parses the compilation unit and sends a stable pointer to the parse result
     back to the Java world -}
foreign export stdcall "parseHaskellCU"
    haskellParseCU :: CString -> IO ( StablePtr ( ParseResult HsModule ) )
haskellParseCU s = do
                 cs <- ( peekCString s )
                 newStablePtr( parseModule cs )

{- | can be used to get information about parse errors; in case there was a 
     ParseFailed, this returns the error message, else the value
     "NO_PROBLEM" -}
foreign export stdcall "getParserError"
    getParserError :: StablePtr ( ParseResult HsModule ) -> IO CString

getParserError sp = do
                   pr <- deRefStablePtr sp
                   newCString ( getError pr )

-- marshalling - modules
------------------------

{-- | returns the name of the module -}
foreign export stdcall "mrsh_moduleName"
    mrsh_moduleName :: StablePtr ( ParseResult HsModule ) -> IO CString
mrsh_moduleName sp = do
                   pr <- deRefStablePtr sp
                   newCString ( getModuleName pr )

{- | returns the line of the module declaration -}
foreign export stdcall "mrsh_moduleLine"
    mrsh_moduleLine :: StablePtr ( ParseResult HsModule ) -> IO Int
mrsh_moduleLine sp = do
                   pr <- deRefStablePtr sp
                   return ( getModuleLine pr )

{- | returns the column of the module declaration -}
foreign export stdcall "mrsh_moduleColumn"
    mrsh_moduleColumn :: StablePtr ( ParseResult HsModule ) -> IO Int
mrsh_moduleColumn sp = do
                     pr <- deRefStablePtr sp
                     return ( getModuleColumn pr )

-- marshalling - export specifications
--------------------------------------

{- | returns the number of export specifications for the module in the parse 
     result -}
foreign export stdcall "mrsh_exportCount"
    mrsh_exportCount :: StablePtr ( ParseResult HsModule ) -> IO Int
mrsh_exportCount sp = do
                    pr <- deRefStablePtr sp
                    return ( getExportCount pr )

{- | returns a type code for the constructor of the export specification at the 
     specified position -}
foreign export stdcall "mrsh_exportSpecificationType"
    mrsh_exportSpecificationType :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_exportSpecificationType sp pos = do
                                    pr <- deRefStablePtr sp
                                    return ( getExportType pr pos )

{-- | returns the name of the export specification at the specified position 
      (the identifier of the exported element) -}
foreign export stdcall "mrsh_exportSpecificationName"
    mrsh_exportSpecificationName :: StablePtr ( ParseResult HsModule ) -> Int -> IO CString
mrsh_exportSpecificationName sp pos = do
                                    pr <- deRefStablePtr sp
                                    newCString ( getExportName pr pos )
                                    
{-- | returnsthe number of exported names in the export specification at the 
      specified position in the module --}
foreign export stdcall "mrsh_exportThingWithCount"
    mrsh_exportThingWithCount :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_exportThingWithCount sp pos = do
                                 pr <- deRefStablePtr sp
                                 return ( getExportThingWithCount pr pos )

{-- | returns the name of the exported element at position pos in the list of 
      the export specification list, assuming that the export specification is 
      a thing-with declaration --}
foreign export stdcall "mrsh_exportThingWithName"
    mrsh_exportThingWithName :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO CString
mrsh_exportThingWithName sp exportPos pos = do
                                          pr <- deRefStablePtr sp
                                          newCString ( getExportThingWithName pr exportPos pos )

-- marshalling - imports
------------------------

{- | returns the number of imports for the module in the parse result -}
foreign export stdcall "mrsh_importCount"
    mrsh_importCount :: StablePtr ( ParseResult HsModule ) -> IO Int
mrsh_importCount sp = do
                    pr <- deRefStablePtr sp
                    return ( getImportCount pr )

{- | returns the name of the imported module at the specified position of the 
     imports list -}
foreign export stdcall "mrsh_importedElement"
    mrsh_importedElement :: StablePtr ( ParseResult HsModule ) -> Int -> IO CString
mrsh_importedElement sp pos = do
                           pr <- deRefStablePtr sp
                           newCString ( getImportedModule pr pos )

{- | returns the line of the import at the specified position of the imports 
     list -}
foreign export stdcall "mrsh_importLine"
    mrsh_importLine :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_importLine sp pos = do
                       pr <- deRefStablePtr sp
                       return ( getImportLine pr pos )

{- | returns the column of the import at the specified position of the imports 
     list -}
foreign export stdcall "mrsh_importColumn"
    mrsh_importColumn :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_importColumn sp pos = do
                         pr <- deRefStablePtr sp
                         return ( getImportColumn pr pos )

{- | returns the count of import specifications for the import at the specified
     position in the imports list of the module in the parse result -}
foreign export stdcall "mrsh_importSpecificationCount"
    mrsh_importSpecificationCount :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_importSpecificationCount sp pos = do
                                     pr <- deRefStablePtr sp
                                     return ( getImportSpecCount pr pos )

{- | whether the import at the specified position in the imports list is
     declared as 'hiding' -}
foreign export stdcall "mrsh_importSpecificationHiding"
    mrsh_importSpecificationHiding :: StablePtr ( ParseResult HsModule ) -> Int -> IO Bool
mrsh_importSpecificationHiding sp pos = do
                                      pr <- deRefStablePtr sp
                                      return ( isImportSpecHiding pr pos )

{- | type code for the constructor of the import specification at the 
     specified position in the list of import specifications for the import
     at the specified position in the imports list of the module -}
foreign export stdcall "mrsh_importSpecificationType"
    mrsh_importSpecificationType :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO Int
mrsh_importSpecificationType sp impPos pos = do
                                           pr <- deRefStablePtr sp
                                           return ( getImportSpecType pr impPos pos )

{- | returns the name of the import specification at the specified position
     in the import specification list of the import at the specified position
     in the import list of the module in the parse result -}
foreign export stdcall "mrsh_importSpecificationName"
    mrsh_importSpecificationName :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO CString
mrsh_importSpecificationName sp impPos pos = do
                                           pr <- deRefStablePtr sp
                                           newCString ( getImportSpecName pr impPos pos )

{- | gets from the module in the parse result the import, the import spec 
     (which is assumed to be thing-with) and returns from that count of 
     components -}
foreign export stdcall "mrsh_importThingWithCount"
    mrsh_importThingWithCount :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO Int
mrsh_importThingWithCount sp impPos pos = do
                                        pr <- deRefStablePtr sp
                                        return ( getImportSpecThingWithCount pr impPos pos )

{- | returns from the module in the parse result the import, the import spec 
     (which is assumed to be thing-with) and from that the component, at
     the specified positions, resp. -}
foreign export stdcall "mrsh_importThingWithName"
    mrsh_importThingWithName :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> Int -> IO CString
mrsh_importThingWithName sp impPos impSpecPos pos = do
                                                  pr <- deRefStablePtr sp
                                                  newCString ( getImportSpecThingWithName pr impPos impSpecPos pos )

-- marshalling - declarations
-----------------------------

{- | returns the number of declarations for the module in the parse result -}
foreign export stdcall "mrsh_declCount"
    mrsh_declCount :: StablePtr ( ParseResult HsModule ) -> IO Int
mrsh_declCount sp = do
                  pr <- deRefStablePtr sp
                  return ( getDeclCount pr )

{- | returns a type code for the constructor to be used for the declaration
     at the specified position in the declarations list -}
foreign export stdcall "mrsh_declType"
    mrsh_declType :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_declType sp pos = do
                     pr <- deRefStablePtr sp
                     return ( getDeclType pr pos )

{-- | returns the name of the declaration at the specified position in the 
      declarations list -}
foreign export stdcall "mrsh_declName"
    mrsh_declName :: StablePtr ( ParseResult HsModule ) -> Int -> IO CString
mrsh_declName sp pos = do
                     pr <- deRefStablePtr sp
                     newCString ( getDeclName pr pos )

{- | returns the line of the declaration at the specified position -}
foreign export stdcall "mrsh_declLine"
    mrsh_declLine :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_declLine sp pos = do
                     pr <- deRefStablePtr sp
                     return ( getDeclLine pr pos )

{- | returns the column of the declaration at the specified position -}
foreign export stdcall "mrsh_declColumn"
    mrsh_declColumn :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_declColumn sp pos = do
                       pr <- deRefStablePtr sp
                       return ( getDeclColumn pr pos )

{- | assuming the declaration at the specified position is a type signature,
     this returns how many identifiers are declared in it -}
foreign export stdcall "mrsh_typeSigIdentifierCount"
    mrsh_typeSigIdentifierCount :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_typeSigIdentifierCount sp pos = do
                                   pr <- deRefStablePtr sp
                                   return ( getIdentifierCount pr pos )

{- | assuming the declaration at the specified position declPos is a type
     signature, return the name of the identifier at position pos in the 
     list of identifiers for which the type is declared -}
foreign export stdcall "mrsh_typeSigIdentifier"
    mrsh_typeSigIdentifier :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO CString
mrsh_typeSigIdentifier sp declPos pos = do
                                      pr <- deRefStablePtr sp
                                      newCString ( getTypeSigIdentifier pr declPos pos )

{- | returns the count of members in the class declaration at the 
     specified position in the declarations list -}
foreign export stdcall "mrsh_classDeclMemberCount"
    mrsh_classDeclMemberCount :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_classDeclMemberCount sp declPos = do
                                     pr <- deRefStablePtr sp
                                     return ( getClassDeclMemberCount pr declPos )

{- | whether the member at the specified position in the members list of the
     declaration at declPos is a type signature -}
foreign export stdcall "mrsh_typeSignMember"
    mrsh_typeSignMember :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO Bool
mrsh_typeSignMember sp declPos memberPos = do
                                         pr <- deRefStablePtr sp
                                         return ( isTypeSigMember pr declPos memberPos )

{- | returns the line number in the source file of the type signature in the 
     class declaration at the specified position in the declarations list -}
foreign export stdcall "mrsh_classDeclTypeSigLine"
    mrsh_classDeclTypeSigLine :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO Int
mrsh_classDeclTypeSigLine sp declPos typeSigPos = do 
                                                pr <- deRefStablePtr sp
                                                return ( getClassDeclTypeSigLine pr declPos typeSigPos )

{- | returns the column in the source file of the type signature in the 
     class declaration at the specified position in the declarations list -}
foreign export stdcall "mrsh_classDeclTypeSigColumn"
    mrsh_classDeclTypeSigColumn :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO Int
mrsh_classDeclTypeSigColumn sp declPos typeSigPos = do
                                                  pr <- deRefStablePtr sp
                                                  return ( getClassDeclTypeSigColumn pr declPos typeSigPos )

{- | returns the number of identifiers of the type signature in the 
     class declaration at the specified position in the declarations list -}
foreign export stdcall "mrsh_classDeclTypeSigIdentifierCount"
    mrsh_classDeclTypeSigIdentifierCount :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO Int
mrsh_classDeclTypeSigIdentifierCount sp declPos typeSigPos = do
                                                           pr <- deRefStablePtr sp
                                                           return ( getClassDeclTypeSigIdentifierCount pr declPos typeSigPos )

{- | returns the identifier at the position pos in the type signature in the 
     class declaration at position declPos in the declarations list -}
foreign export stdcall "mrsh_classDeclTypeSigIdentifierName"
    mrsh_classDeclTypeSigIdentifierName :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> Int -> IO CString
mrsh_classDeclTypeSigIdentifierName sp declPos typeSigPos identifierPos = do
                                                                        pr <- deRefStablePtr sp
                                                                        newCString ( getClassDeclTypeSigIdentifierName pr declPos typeSigPos identifierPos )

{- | returns the precedence level of the infix declaration at the position
     declPos in the declarations list -}
foreign export stdcall "mrsh_infixDeclPrecedenceLevel"
    mrsh_infixDeclPrecedenceLevel :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_infixDeclPrecedenceLevel sp declPos = do
                                         pr <- deRefStablePtr sp
                                         return ( getInfixDeclPrecendenceLevel pr declPos )

{- | returns the associativity of the infix declaration at the position
     declPos in the declarations list -}
foreign export stdcall "mrsh_infixDeclAssociativity"
    mrsh_infixDeclAssociativity :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_infixDeclAssociativity sp declPos = do
                                       pr <- deRefStablePtr sp
                                       return ( getInfixDeclAssociativity pr declPos )

{- | returns the count of operators in the infix declaration at the position
     declPos in the declarations list -}
foreign export stdcall "mrsh_infixDeclOperatorCount"
    mrsh_infixDeclOperatorCount :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_infixDeclOperatorCount sp declPos = do
                                       pr <- deRefStablePtr sp
                                       return ( getInfixDeclOperatorCount pr declPos )

{- | returns the name of the operator at position pos in the operators list 
     in the infix declaration at the position declPos in the declarations 
     list -}
foreign export stdcall "mrsh_infixDeclOperator"
    mrsh_infixDeclOperator :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO CString
mrsh_infixDeclOperator sp declPos opPos = do
                                        pr <- deRefStablePtr sp
                                        newCString ( getInfixDeclOperator pr declPos opPos )

{- | returns the count of constructors in the data declaration at position
     declPos in the declarations list -}
--  extern int mrsh_dataDeclConstructorCount( HsStablePtr parseResult, 
--                                            jint declPos );
foreign export stdcall "mrsh_dataDeclConstructorCount"
    mrsh_dataDeclConstructorCount :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_dataDeclConstructorCount sp declPos = do
                                         pr <- deRefStablePtr sp
                                         return ( getDataDeclConstructorCount pr declPos )

{- | returns the name of the constructor at position constPos in the data 
     declaration at position declPos in the declarations list -}
foreign export stdcall "mrsh_dataDeclConstructorName"
    mrsh_dataDeclConstructorName :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO CString
mrsh_dataDeclConstructorName sp declPos constPos = do
                                                 pr <- deRefStablePtr sp
                                                 newCString ( getDataDeclConstructorName pr declPos constPos )

{- | returns the line in the source file of the constructor at position 
     constPos in the data declaration at position declPos in the declarations 
     list -}
foreign export stdcall "mrsh_dataDeclConstructorLine"
    mrsh_dataDeclConstructorLine :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO Int
mrsh_dataDeclConstructorLine sp declPos constPos = do
                                                 pr <- deRefStablePtr sp
                                                 return ( getDataDeclConstructorLine pr declPos constPos )
    
{- | returns the column in the source file of the constructor at position 
     constPos in the data declaration at position declPos in the declarations 
     list -}
foreign export stdcall "mrsh_dataDeclConstructorColumn"
    mrsh_dataDeclConstructorColumn :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO Int
mrsh_dataDeclConstructorColumn sp declPos constPos = do
                                                   pr <- deRefStablePtr sp
                                                   return ( getDataDeclConstructorColumn pr declPos constPos )

{- | returns the count of matches in the function binding at position
     declPos in the declarations list -}
foreign export stdcall "mrsh_functionBindingMatchCount"
    mrsh_functionBindingMatchCount :: StablePtr ( ParseResult HsModule ) -> Int -> IO Int
mrsh_functionBindingMatchCount sp declPos = do
                                          pr <- deRefStablePtr sp
                                          return ( getFunctionBindingMatchCount pr declPos )
    
{- | returns the name of the match at position  matchPos in the function 
     binding at position declPos in the declarations list -}
foreign export stdcall "mrsh_functionBindingMatchName"
    mrsh_functionBindingMatchName :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO CString
mrsh_functionBindingMatchName sp declPos matchPos = do
                                                  pr <- deRefStablePtr sp
                                                  newCString ( getFunctionBindingMatchName pr declPos matchPos )

{- | returns the line in the source file of the match at position 
     matchPos in the function binding at position declPos in the declarations 
     list -}
foreign export stdcall "mrsh_functionBindingMatchLine"
    mrsh_functionBindingMatchLine :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO Int
mrsh_functionBindingMatchLine sp declPos matchPos = do
                                                  pr <- deRefStablePtr sp
                                                  return ( getFunctionBindingMatchLine pr declPos matchPos )

{- | returns the column in the source file of the match at position 
     matchPos in the function binding at position declPos in the declarations 
     list -}
foreign export stdcall "mrsh_functionBindingMatchColumn"
    mrsh_functionBindingMatchColumn :: StablePtr ( ParseResult HsModule ) -> Int -> Int -> IO Int
mrsh_functionBindingMatchColumn sp declPos matchPos = do
                                                    pr <- deRefStablePtr sp
                                                    return ( getFunctionBindingMatchColumn pr declPos matchPos )


-- internally used functions
----------------------------

getError :: ParseResult HsModule -> String
getError ( ParseOk _ ) = "NO_PROBLEM"
getError ( ParseFailed srcLoc msg ) = msg ++ " " ++ encodeLoc srcLoc
  where encodeLoc ( SrcLoc _ line col ) = "(" ++ show line ++ "," ++ show col ++ ")"

-- modules
getModuleName :: ParseResult HsModule -> String
getModuleName ( ParseOk m ) = extractName m
  where extractName ( HsModule _ ( Module s ) _ _ _ ) = s

getModuleLine :: ParseResult HsModule -> Int
getModuleLine ( ParseOk ( HsModule srcLoc _ _ _ _ ) ) = getSrcLine srcLoc

getModuleColumn :: ParseResult HsModule -> Int
getModuleColumn ( ParseOk ( HsModule srcLoc _ _ _ _ ) ) = getSrcColumn srcLoc

-- export specifications
getExportCount :: ParseResult HsModule -> Int
getExportCount ( ParseOk ( HsModule _ _ ( Just exportSpecs ) _ _ ) ) = length exportSpecs
getExportCount ( ParseOk ( HsModule _ _ Nothing _ _ ) ) = 0

getExportType :: ParseResult HsModule -> Int -> Int
getExportType ( ParseOk ( HsModule _ _ ( Just exports ) _ _ ) ) pos = getExportSpecTypeId( exports !! pos )

getExportName :: ParseResult HsModule -> Int -> String
getExportName ( ParseOk ( HsModule _ _ ( Just exports ) _ _ ) ) pos = extractName( exports !! pos )
  where extractName ( HsEVar qName ) = getQNameString qName
        extractName ( HsEAbs qName ) = getQNameString qName
        extractName ( HsEThingAll qName ) = getQNameString qName
        extractName ( HsEThingWith qName _ ) = getQNameString qName
        extractName ( HsEModuleContents ( Module s )  ) = s

getExportThingWithCount :: ParseResult HsModule -> Int -> Int
getExportThingWithCount ( ParseOk ( HsModule _ _ ( Just exports ) _ _ ) ) pos = extractCount( exports !! pos )
  where extractCount ( HsEThingWith _ names ) = length names
  
getExportThingWithName :: ParseResult HsModule -> Int -> Int -> String
getExportThingWithName ( ParseOk ( HsModule _ _ ( Just exports ) _ _ ) ) exportPos pos = extractName ( exports !! exportPos ) pos
  where extractName ( HsEThingWith _ names ) pos = getCNameString ( names !! pos )

-- imports
getImportCount :: ParseResult HsModule -> Int
getImportCount ( ParseOk ( HsModule _ _ _ impdecls _ ) ) = length impdecls

getImportedModule :: ParseResult HsModule -> Int -> String
getImportedModule ( ParseOk ( HsModule _ _ _ impdecls _ ) ) pos = extractName( impdecls !! pos )
  where extractName ( HsImportDecl _ ( Module s ) _ _ _ ) = s

getImportLine :: ParseResult HsModule -> Int -> Int
getImportLine ( ParseOk ( HsModule _ _ _ impdecls _ ) ) pos = extractLine( impdecls !! pos )
  where extractLine ( HsImportDecl srcLoc _ _ _ _ ) = getSrcLine srcLoc

getImportColumn :: ParseResult HsModule -> Int -> Int
getImportColumn ( ParseOk ( HsModule _ _ _ impdecls _ ) ) pos = extractColumn( impdecls !! pos )
  where extractColumn ( HsImportDecl srcLoc _ _ _ _ ) = getSrcColumn srcLoc

getImportSpecCount :: ParseResult HsModule -> Int -> Int
getImportSpecCount ( ParseOk ( HsModule _ _ _ impdecls _ ) ) pos = getSpecCount( impdecls !! pos )
  where getSpecCount ( HsImportDecl _ _ _ _ ( Just ( _, impSpecs ) ) ) = length impSpecs
        getSpecCount ( HsImportDecl _ _ _ _ Nothing ) = 0
        
isImportSpecHiding :: ParseResult HsModule -> Int -> Bool
isImportSpecHiding pr pos = isHiding( getImpDecl pr pos )
  where isHiding ( HsImportDecl _ _ _ _ ( Just ( hiding, _ ) ) ) = hiding

getImportSpecType :: ParseResult HsModule -> Int -> Int -> Int
getImportSpecType ( ParseOk ( HsModule _ _ _ impdecls _ ) ) impPos pos = getSpecType ( impdecls !! impPos ) pos
  where getSpecType ( HsImportDecl _ _ _ _ ( Just ( _, impSpecs ) ) ) pos = getImportSpecTypeId( impSpecs !! pos )
  
getImportSpecName :: ParseResult HsModule -> Int -> Int -> String
getImportSpecName ( ParseOk ( HsModule _ _ _ impdecls _ ) ) impPos pos = extractName ( impdecls !! impPos ) pos
  where extractName ( HsImportDecl _ _ _ _ ( Just ( _, impSpecs ) ) ) pos = extractSpecName ( impSpecs !! pos )
        extractSpecName ( HsIVar name ) = getNameString name
        extractSpecName ( HsIAbs name ) = getNameString name
        extractSpecName ( HsIThingAll name ) = getNameString name
        extractSpecName ( HsIThingWith name _ ) = getNameString name

getImportSpecThingWithCount :: ParseResult HsModule -> Int -> Int -> Int
getImportSpecThingWithCount pr impPos pos = extractComponentCount ( getImpDecl pr impPos ) pos
  where extractComponentCount ( HsImportDecl _ _ _ _ ( Just ( _, impSpecs ) ) ) pos = getComponentCount ( impSpecs !! pos )
        getComponentCount( HsIThingWith _ comps ) = length comps

getImportSpecThingWithName :: ParseResult HsModule -> Int -> Int -> Int -> String
getImportSpecThingWithName pr impPos impSpecPos pos = extractComponentName ( getImpSpecDecl pr impPos impSpecPos ) pos
  where extractComponentName ( HsIThingWith _ names ) pos = getCNameString ( names !! pos )

getImpDecl :: ParseResult HsModule -> Int -> HsImportDecl
getImpDecl ( ParseOk ( HsModule _ _ _ impDecls _ ) ) impPos = impDecls !! impPos

getImpSpecDecl :: ParseResult HsModule -> Int -> Int -> HsImportSpec
getImpSpecDecl pr impPos pos = extractSpecDecl ( getImpDecl pr impPos ) pos
  where extractSpecDecl ( HsImportDecl _ _ _ _ ( Just ( _, impSpecs ) ) ) pos = impSpecs !! pos


-- declarations
getDeclCount :: ParseResult HsModule -> Int
getDeclCount ( ParseOk ( HsModule _ _ _ _ decls ) ) = length decls

getDeclType :: ParseResult HsModule -> Int -> Int
getDeclType ( ParseOk ( HsModule _ _ _ _ decls ) ) pos = getDeclTypeId( decls !! pos )

getDeclName :: ParseResult HsModule -> Int -> String
getDeclName ( ParseOk ( HsModule _ _ _ _ decls ) ) pos = extractName( decls !! pos )
  where extractName ( HsTypeDecl _ name _ _ ) = getNameString name
        extractName ( HsDataDecl _ _ name _ _ _ ) = getNameString name
        -- omitting HsInfixDecl - no name to get directly here
        extractName ( HsNewTypeDecl _ _ name _ _ _ ) = getNameString name
        extractName ( HsClassDecl _ _ name _ _ ) = getNameString name
        extractName ( HsInstDecl _ _ name _ _ ) = getQNameString name
        -- omitting HsDefaultDecl - no interesting info to get here
        -- omitting HsTypeSig (because it may have more than one identifier,
        --                     it gets a special treatment)
        -- omitting HsFunBind - no interesting info to get here
        -- omitting HsPatBind - no interesting info to get here

getDeclLine :: ParseResult HsModule -> Int -> Int
getDeclLine ( ParseOk ( HsModule _ _ _ _ decls ) ) pos = extractLine( decls !! pos )
  where extractLine decl = getSrcLine( extractSrcLoc decl )

getDeclColumn :: ParseResult HsModule -> Int -> Int
getDeclColumn ( ParseOk ( HsModule _ _ _ _ decls ) ) pos = extractColumn( decls !! pos )
  where extractColumn decl = getSrcColumn( extractSrcLoc decl )

extractSrcLoc :: HsDecl -> SrcLoc
extractSrcLoc( HsTypeDecl srcLoc _ _ _ ) = srcLoc
extractSrcLoc( HsDataDecl srcLoc _ _ _ _ _ ) = srcLoc
extractSrcLoc( HsInfixDecl srcLoc _ _ _ ) = srcLoc
extractSrcLoc( HsNewTypeDecl srcLoc _ _ _ _ _ ) = srcLoc
extractSrcLoc( HsClassDecl srcLoc _ _ _ _ ) = srcLoc
extractSrcLoc( HsInstDecl srcLoc _ _ _ _ ) = srcLoc
extractSrcLoc( HsDefaultDecl srcLoc _ ) = srcLoc
extractSrcLoc( HsTypeSig srcLoc _ _ ) = srcLoc
-- omitting HsFunBind, which gets special treatment
extractSrcLoc( HsPatBind srcLoc _ _ _ ) = srcLoc

getIdentifierCount :: ParseResult HsModule -> Int -> Int
getIdentifierCount ( ParseOk ( HsModule _ _ _ _ decls ) ) pos = extractIdentifierCount( decls !! pos )
  where extractIdentifierCount ( HsTypeSig _ names _ ) = length names
  
getTypeSigIdentifier :: ParseResult HsModule -> Int -> Int -> String
getTypeSigIdentifier( ParseOk ( HsModule _ _ _ _ decls ) ) declPos pos = extractIdentifierCount ( decls !! declPos ) pos
  where extractIdentifierCount ( HsTypeSig _ names _ ) pos = getNameString ( names !!pos )

getClassDeclMemberCount :: ParseResult HsModule -> Int -> Int
getClassDeclMemberCount pr declPos = length ( getMemberList pr declPos )

isTypeSigMember :: ParseResult HsModule -> Int -> Int -> Bool
isTypeSigMember pr declPos memberPos = isTS ( ( getMemberList pr declPos ) !! memberPos )
  where isTS ( HsTypeSig _ _ _ ) = True
        isTS _ = False

getClassDeclTypeSigLine :: ParseResult HsModule -> Int -> Int -> Int
getClassDeclTypeSigLine pr declPos typeSigPos = extractLine ( ( getMemberList pr declPos ) !! typeSigPos )
  where extractLine decl = getSrcLine( extractSrcLoc decl )
  
getClassDeclTypeSigColumn :: ParseResult HsModule -> Int -> Int -> Int
getClassDeclTypeSigColumn pr declPos typeSigPos = extractColumn ( ( getMemberList pr declPos ) !! typeSigPos )
  where extractColumn decl = getSrcColumn( extractSrcLoc decl )

getClassDeclTypeSigIdentifierCount :: ParseResult HsModule -> Int -> Int -> Int
getClassDeclTypeSigIdentifierCount pr declPos typeSigPos = extractIdentifierCount ( ( getMemberList pr declPos ) !! typeSigPos )
  where extractIdentifierCount ( HsTypeSig _ names _ ) = length names

getClassDeclTypeSigIdentifierName :: ParseResult HsModule -> Int -> Int -> Int -> String
getClassDeclTypeSigIdentifierName pr declPos typeSigPos identifierPos = extractIdentifier ( ( getMemberList pr declPos ) !! typeSigPos ) identifierPos
  where extractIdentifier ( HsTypeSig _ names _ ) pos = getNameString ( names !! pos )

getMemberList :: ParseResult HsModule -> Int -> [HsDecl]
getMemberList pr declPos = extractMemberList ( getDecl pr declPos )
  where extractMemberList ( HsClassDecl _ _ _ _ members ) = members
  
getInfixDeclPrecendenceLevel :: ParseResult HsModule -> Int -> Int
getInfixDeclPrecendenceLevel pr declPos = extractPLevel ( getDecl pr declPos )
  where extractPLevel ( HsInfixDecl _ _ level _ ) = level

getInfixDeclAssociativity :: ParseResult HsModule -> Int -> Int
getInfixDeclAssociativity pr declPos = extractAssoc ( getDecl pr declPos )
  where extractAssoc ( HsInfixDecl _ assoc _ _ ) = getAssocType assoc

getInfixDeclOperatorCount :: ParseResult HsModule -> Int -> Int
getInfixDeclOperatorCount pr declPos = extractCount ( getDecl pr declPos )
  where extractCount ( HsInfixDecl _ _ _ ops ) = length ops

getInfixDeclOperator :: ParseResult HsModule -> Int -> Int -> String
getInfixDeclOperator pr declPos opPos = extractOpName ( getDecl pr declPos ) opPos
  where extractOpName ( HsInfixDecl _ _ _ ops ) opPos =  getOpName ( ops !! opPos )
        getOpName ( HsVarOp op ) = getNameString op
        getOpName ( HsConOp op ) = getNameString op
        
getDataDeclConstructorCount :: ParseResult HsModule -> Int -> Int 
getDataDeclConstructorCount pr declPos = extractCount( getDecl pr declPos )
  where extractCount ( HsDataDecl _ _ _ _ conDecls _ ) = length conDecls

getDataDeclConstructorName :: ParseResult HsModule -> Int -> Int -> String
getDataDeclConstructorName pr declPos constPos = extractName ( getConst pr declPos constPos  )
  where extractName ( HsConDecl _ name _ ) = getNameString name
        extractName ( HsRecDecl _ name _ ) = getNameString name

getDataDeclConstructorLine :: ParseResult HsModule -> Int -> Int -> Int
getDataDeclConstructorLine pr declPos constPos = extractLine ( getConst pr declPos constPos  )
  where extractLine ( HsConDecl srcLoc _ _ ) = getSrcLine srcLoc
        extractLine ( HsRecDecl srcLoc _ _ ) = getSrcLine srcLoc

getDataDeclConstructorColumn :: ParseResult HsModule -> Int -> Int -> Int
getDataDeclConstructorColumn pr declPos constPos = extractColumn ( getConst pr declPos constPos  )
  where extractColumn ( HsConDecl srcLoc _ _ ) = getSrcColumn srcLoc
        extractColumn ( HsRecDecl srcLoc _ _ ) = getSrcColumn srcLoc

getConst :: ParseResult HsModule -> Int -> Int -> HsConDecl
getConst pr declPos constPos = extractConst ( getDecl pr declPos ) constPos
  where extractConst ( HsDataDecl _ _ _ _ conDecls _ ) constPos = conDecls !! constPos

getFunctionBindingMatchCount :: ParseResult HsModule -> Int -> Int 
getFunctionBindingMatchCount pr declPos = extractCount( getDecl pr declPos )
  where extractCount ( HsFunBind matches ) = length matches
    
getFunctionBindingMatchName :: ParseResult HsModule -> Int -> Int -> String
getFunctionBindingMatchName pr declPos matchPos = extractName ( getMatch pr declPos matchPos  )
  where extractName ( HsMatch _ name _ _ _ ) = getNameString name

getFunctionBindingMatchLine :: ParseResult HsModule -> Int -> Int -> Int
getFunctionBindingMatchLine pr declPos matchPos = extractLine ( getMatch pr declPos matchPos  )
  where extractLine ( HsMatch srcLoc _ _ _ _ ) = getSrcLine srcLoc

getFunctionBindingMatchColumn :: ParseResult HsModule -> Int -> Int -> Int
getFunctionBindingMatchColumn pr declPos matchPos = extractColumn ( getMatch pr declPos matchPos  )
  where extractColumn ( HsMatch srcLoc _ _ _ _ ) = getSrcColumn srcLoc

getMatch :: ParseResult HsModule -> Int -> Int -> HsMatch
getMatch pr declPos matchPos = extractMatch ( getDecl pr declPos ) matchPos
  where extractMatch ( HsFunBind matches ) matchPos = matches !! matchPos


-- helping functions
--------------------

getDecl :: ParseResult HsModule -> Int -> HsDecl
getDecl ( ParseOk ( HsModule _ _ _ _ decls ) ) declPos = decls !! declPos

getSrcLine :: SrcLoc -> Int
getSrcLine ( SrcLoc _ line _ ) = line

getSrcColumn :: SrcLoc -> Int
getSrcColumn ( SrcLoc _ _ column ) = column

getNameString :: HsName -> String
getNameString ( HsIdent s ) = s
getNameString ( HsSymbol s ) = s

getCNameString :: HsCName -> String
getCNameString ( HsVarName s ) = getNameString s
getCNameString ( HsConName s ) = getNameString s

getQNameString :: HsQName -> String
getQNameString ( Qual ( Module s )  name ) = s ++ "." ++ getNameString name
getQNameString ( UnQual name ) = getNameString name
getQNameString s = show s

-- element types
----------------

getDeclTypeId :: HsDecl -> Int
  
getDeclTypeId ( HsTypeDecl _ _ _ _ ) = 0
getDeclTypeId ( HsDataDecl _ _ _ _ _ _ ) = 1
getDeclTypeId ( HsInfixDecl _ _ _ _ ) = 2
getDeclTypeId ( HsNewTypeDecl _ _ _ _ _ _ ) = 3
getDeclTypeId ( HsClassDecl _ _ _ _ _ ) = 4
getDeclTypeId ( HsInstDecl _ _ _ _ _ )  = 5
getDeclTypeId ( HsDefaultDecl _ _ ) = 6
getDeclTypeId ( HsTypeSig _ _ _ ) = 7
getDeclTypeId ( HsFunBind _ ) = 8
getDeclTypeId ( HsPatBind _ _ _ _ ) = 9

getExportSpecTypeId :: HsExportSpec -> Int

getExportSpecTypeId ( HsEVar _ ) = 10
getExportSpecTypeId ( HsEAbs _ ) = 11
getExportSpecTypeId ( HsEThingAll _ ) = 12
getExportSpecTypeId ( HsEThingWith _ _ ) = 13
getExportSpecTypeId ( HsEModuleContents  _ ) = 14

getImportSpecTypeId :: HsImportSpec -> Int

getImportSpecTypeId ( HsIVar _ ) = 15
getImportSpecTypeId ( HsIAbs _ ) = 16
getImportSpecTypeId ( HsIThingAll _ ) = 17
getImportSpecTypeId ( HsIThingWith _ _ ) = 18

getAssocType :: HsAssoc -> Int

getAssocType HsAssocNone = 0
getAssocType HsAssocLeft = 1
getAssocType HsAssocRight = 2

