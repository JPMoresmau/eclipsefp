// Copyright (c) 2004-2005 by Leif Frenzel
// See http://leiffrenzel.de
//
// bridge.c - contains the implementation of bridge code (needed for the JNI)
//            basically doing some argument conversion and forwarding to 
//            Haskell functions
#include <jni.h>
#include <HsFFI.h>
#include "marshal.h"

  extern void __stginit_ParserImpl( void );
  extern HsStablePtr parseHaskellCU( const char* );
  extern char* getParserError( HsStablePtr );

  static char *args[] = { "ghcDll", 0 };
  static int argc = 1;
  static char **argv = args;


  // drive the Haskell runtime system
  ///////////////////////////////////

  // haskellInit()
  JNIEXPORT void JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_haskellInit( JNIEnv *e, jclass c ) {
    hs_init( &argc, &argv );
    hs_add_root( __stginit_ParserImpl );
  }

  // haskellExit()
  JNIEXPORT void JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_haskellExit( JNIEnv *e, jclass c ) {
    hs_exit();
  }


  // maintain stable pointer to a compilation unit's parse result
  ///////////////////////////////////////////////////////////////

  // haskellParseCU( jstring )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_haskellParseCU( JNIEnv* env, 
                                                jclass c, 
                                                jstring jstr ) {
    const char* str = ( *env )->GetStringUTFChars( env, jstr, 0 );
    jint result = ( jint )parseHaskellCU( str );
    ( *env )->ReleaseStringUTFChars( env, jstr, str );
    return result;
  }

  // haskellFreeHandle( jint )
  JNIEXPORT void JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_haskellFreeHandle( JNIEnv *e, 
                                                   jclass c, 
                                                   jint handle ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    hs_free_stable_ptr( sp );
  }

  // getErrorMessage( jint )
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getErrorMessage( JNIEnv* env, 
                                                    jclass c, 
                                                    jint handle ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = getParserError( sp );
    return ( *env )->NewStringUTF( env, name );
  }


  // forwarding calls from Java to Haskell
  ////////////////////////////////////////

  // modules

  // getModuleName( jint )
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getModuleName( JNIEnv* env, jclass c, jint handle ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_moduleName( sp );
    return ( *env )->NewStringUTF( env, name );
  }

  // getModuleLine( jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getModuleLine( JNIEnv* env, jclass c, jint handle ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_moduleLine( sp );
  }

  // getModuleColumn( jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getModuleColumn(JNIEnv* env, jclass c, jint handle) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_moduleColumn( sp );
  }

  // exports
  
  // getExportCount( jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getExportCount( JNIEnv* env, jclass c, jint handle ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_exportCount( sp );
  }

  // getExportSpecificationType( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getExportSpecificationType( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_exportSpecificationType( sp, pos );
  }

  // getExportSpecificationName( jint, jint )
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getExportSpecificationName( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_exportSpecificationName( sp, pos );
    return ( *env )->NewStringUTF( env, name );
  }

  // getExportThingWithCount( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getExportThingWithCount( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_exportThingWithCount( sp, pos );
  }

  // getExportThingWithName( jint, jint, jint )
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getExportThingWithName( JNIEnv* env, jclass c, jint handle, jint exportPos, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_exportThingWithName( sp, exportPos, pos );
    return ( *env )->NewStringUTF( env, name );
  }

  // imports

  // getImportCount( jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getImportCount( JNIEnv* env, jclass c, jint handle ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_importCount( sp );
  }

  // getImportedElement( jint, jint )
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getImportedElement( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_importedElement( sp, pos );
    return ( *env )->NewStringUTF( env, name );
  }

  // getImportLine( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getImportLine( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_importLine( sp, pos );
  }

  // getImportColumn( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getImportColumn( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_importColumn( sp, pos );
  }
  
  // getImportSpecificationCount( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getImportSpecificationCount( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_importSpecificationCount( sp, pos );
  }

  // isImportSpecificationHiding( jint, jint )
  JNIEXPORT jboolean JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_isImportSpecificationHiding( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_importSpecificationHiding( sp, pos );
  }

  // getImportSpecificationType( jint, jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getImportSpecificationType( JNIEnv* env, jclass c, jint handle, jint impPos, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_importSpecificationType( sp, impPos, pos );
  }
                                                
  // getImportSpecificationName( jint, jint, jint )
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getImportSpecificationName( JNIEnv* env, jclass c, jint handle, jint impPos, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_importSpecificationName( sp, impPos, pos );
    return ( *env )->NewStringUTF( env, name );
  }
  
  // getImportThingWithCount( jint, jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getImportThingWithCount( JNIEnv* env, jclass c, jint handle, jint impPos, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_importThingWithCount( sp, impPos, pos );
  }

  // getImportThingWithName( jint, jint, jint, jint )
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getImportThingWithName( JNIEnv* env, jclass c, jint handle, jint impPos, jint impSpecPos, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_importThingWithName( sp, impPos, impSpecPos, pos );
    return ( *env )->NewStringUTF( env, name );
  }
  
  // declarations

  // getDeclCount( jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getDeclCount( JNIEnv* env, 
                                              jclass c, 
                                              jint handle ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_declCount( sp );
  }

  // getDeclType( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getDeclType( JNIEnv* env, 
                                             jclass c, 
                                             jint handle, 
                                             jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_declType( sp, pos );
  }

  // getDeclName( jint, jint )
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getDeclName( JNIEnv* env, 
                                                jclass c, 
                                                jint handle, 
                                                jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_declName( sp, pos );
    return ( *env )->NewStringUTF( env, name );
  }

  // getDeclLine( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getDeclLine( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_declLine( sp, pos );
  }

  // getDeclColumn( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getDeclColumn( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_declColumn( sp, pos );
  }

  // getTypeSigIdentifierCount( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getTypeSigIdentifierCount( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_typeSigIdentifierCount( sp, pos );
  }

  // getTypeSigIdentifier( jint, jint, jint );
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getTypeSigIdentifier( JNIEnv* env, jclass c, jint handle, jint declPos, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_typeSigIdentifier( sp, declPos, pos );
    return ( *env )->NewStringUTF( env, name );
  }

  // getClassDeclMemberCount( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getClassDeclMemberCount( JNIEnv* env, jclass c, jint handle, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_classDeclMemberCount( sp, pos );
  }

  // isTypeSigMember( jint, jint, jint )
  JNIEXPORT jboolean JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_isTypeSigMember( JNIEnv* env, jclass c, jint handle, jint declPos, jint memberPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_typeSignMember( sp, declPos, memberPos );
  }

  // getClassDeclTypeSigLine( jint, jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getClassDeclTypeSigLine( JNIEnv* env, jclass c, jint handle, jint declPos, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_classDeclTypeSigLine( sp, declPos, pos );
  }

  // getClassDeclTypeSigColumn( jint, jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getClassDeclTypeSigColumn( JNIEnv* env, 
                                                           jclass c, 
                                                           jint handle, 
                                                           jint declPos, 
                                                           jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_classDeclTypeSigColumn( sp, declPos, pos );
  }

  // getClassDeclTypeSigIdentifierCount( jint, jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getClassDeclTypeSigIdentifierCount( JNIEnv* env, 
                                                                    jclass c, 
                                                                    jint handle, 
                                                                    jint declPos,
                                                                    jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_classDeclTypeSigIdentifierCount( sp, declPos, pos );
  }

  // getClassDeclTypeSigIdentifierName( jint, jint, jint, jint );
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getClassDeclTypeSigIdentifierName( JNIEnv* env, 
                                                                      jclass c, 
                                                                      jint handle, 
                                                                      jint declPos, 
                                                                      jint typeSigPos, 
                                                                      jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_classDeclTypeSigIdentifierName( sp, declPos, typeSigPos, pos );
    return ( *env )->NewStringUTF( env, name );
  }

  // getInfixDeclPrecedenceLevel( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getInfixDeclPrecedenceLevel( JNIEnv* env, 
                                             jclass c, 
                                             jint handle, 
                                             jint declPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_infixDeclPrecedenceLevel( sp, declPos );
  }

  // getInfixDeclAssociativity( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getInfixDeclAssociativity( JNIEnv* env, 
                                             jclass c, 
                                             jint handle, 
                                             jint declPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_infixDeclAssociativity( sp, declPos );
  }
  
  // getInfixDeclOperatorCount( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getInfixDeclOperatorCount( JNIEnv* env, 
                                             jclass c, 
                                             jint handle, 
                                             jint declPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_infixDeclOperatorCount( sp, declPos );
  }

  // getInfixDeclOperator( jint, jint, jint )
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getInfixDeclOperator( JNIEnv* env, jclass c, jint handle, jint declPos, jint pos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_infixDeclOperator( sp, declPos, pos );
    return ( *env )->NewStringUTF( env, name );
  }

  // getDataDeclConstructorCount( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getDataDeclConstructorCount( JNIEnv* env, jclass c, jint handle, jint declPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_dataDeclConstructorCount( sp, declPos );
  }

  // getDataDeclConstructorName( jint, jint, jint );
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getDataDeclConstructorName( JNIEnv* env, jclass c, jint handle, jint declPos, jint constPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_dataDeclConstructorName( sp, declPos, constPos );
    return ( *env )->NewStringUTF( env, name );
  }


  // getDataDeclConstructorLine( jint, jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getDataDeclConstructorLine( JNIEnv* env, jclass c, jint handle, jint declPos, jint constPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_dataDeclConstructorLine( sp, declPos, constPos );
  }

  // getDataDeclConstructorColumn( jint, jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getDataDeclConstructorColumn( JNIEnv* env, 
                                                           jclass c, 
                                                           jint handle, 
                                                           jint declPos, 
                                                           jint constPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_dataDeclConstructorColumn( sp, declPos, constPos );
  }

  // getFunctionBindingMatchCount( jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getFunctionBindingMatchCount( JNIEnv* env, jclass c, jint handle, jint declPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_functionBindingMatchCount( sp, declPos );
  }

  // getFunctionBindingMatchName( jint, jint, jint );
  JNIEXPORT jstring JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getFunctionBindingMatchName( JNIEnv* env, jclass c, jint handle, jint declPos, jint matchPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    const char* name = mrsh_functionBindingMatchName( sp, declPos, matchPos );
    return ( *env )->NewStringUTF( env, name );
  }


  // getFunctionBindingMatchLine( jint, jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getFunctionBindingMatchLine( JNIEnv* env, jclass c, jint handle, jint declPos, jint matchPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_functionBindingMatchLine( sp, declPos, matchPos );
  }

  // getFunctionBindingMatchColumn( jint, jint, jint )
  JNIEXPORT jint JNICALL Java_net_sf_eclipsefp_haskell_core_parser_NativeParser_getFunctionBindingMatchColumn( JNIEnv* env, 
                                                           jclass c, 
                                                           jint handle, 
                                                           jint declPos, 
                                                           jint matchPos ) {
    HsStablePtr sp = ( HsStablePtr )handle;
    return mrsh_functionBindingMatchColumn( sp, declPos, matchPos );
  }
