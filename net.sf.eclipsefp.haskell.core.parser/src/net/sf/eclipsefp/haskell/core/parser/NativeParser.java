// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.parser;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.*;


/** <p>A singleton to access the native parser. The native library is
  * initialized when the singleton instance is accessed for the first 
  * time. It must, however, disposed explicitly when no longer needed.</p>
  *
  * @author Leif Frenzel
  */
public class NativeParser implements IElementTypes {
  
  private static final String NO_PROBLEM = "NO_PROBLEM";
  
  private static NativeParser _instance;
  private static boolean disposed;


  private NativeParser() {
    // prevent instantiation from outside
    try {
      System.loadLibrary( "nativeparser" );
      haskellInit();
      disposed = false;
    } catch( Throwable thr ) {
      ParserPlugin.log( "Could not initialize native parser library.", thr );
      disposed = true;
    }
  }


  // API
  //////

  public synchronized static NativeParser getInstance() {
    if( disposed ) {
      String msg = "The native parser is already disposed.";
      throw new IllegalStateException( msg );
    }

    if( _instance == null ) {
      _instance = new NativeParser();
    }
    
    if( disposed ) {
      _instance = null;
      String msg = "Initialization of the native parser failed.";
      throw new IllegalStateException( msg );
    }
    return _instance;
  }

  public void dispose() {
    haskellExit();
    disposed = true;
  }

  public CompilationUnit parseCompilationUnit( final String content, 
                                               final IFile file ) 
                                                          throws CoreException {
    int handle = haskellParseCU( content );
    
    String errorMessage = getErrorMessage( handle );
    if( !errorMessage.equals( NO_PROBLEM ) ) {
      fireCoreException( errorMessage );
    }
    
    CompilationUnit result = null;
    try {
      result = new CompilationUnit( handle, file );
    } finally {
      haskellFreeHandle( handle );
    }
    return result;
  }
  

  // helping methods
  //////////////////
  
  private void fireCoreException( final String msg ) throws CoreException {
    String pluginId = ParserPlugin.getPluginId();
    IStatus status = new Status( IStatus.ERROR, pluginId, 0, msg, null );
    throw new CoreException( status );
  }

  
  // native methods
  /////////////////

  // marshalling - modules
  static native String getModuleName( int handle );
  static native int getModuleLine( int handle );
  static native int getModuleColumn( int handle );
  // marshalling - export specifications
  static native int getExportCount( int handle );
  static native int getExportSpecificationType( int handle, int pos );
  static native String getExportSpecificationName( int handle, int pos );
  static native int getExportThingWithCount( int handle, int exportPos );
  static native String getExportThingWithName( int handle, 
                                               int exportPos, 
                                               int pos );
  // marshalling - imports
  static native int getImportCount( int handle );
  static native String getImportedElement( int handle, int pos );
  static native int getImportLine( int handle, int pos );
  static native int getImportColumn( int handle, int pos );
  static native int getImportSpecificationCount( int handle, int pos );
  static native boolean isImportSpecificationHiding( int handle, int pos );
  static native int getImportSpecificationType( int handle, 
                                                int impPos, 
                                                int pos );
  static native String getImportSpecificationName( int handle, 
                                                   int impPos, 
                                                   int pos );
  static native int getImportThingWithCount( int handle, int impPos, int pos );
  static native String getImportThingWithName( int handle,
                                               int impPos, 
                                               int impSpecPos,
                                               int pos );
  // marshalling - declarations
  static native int getDeclCount( int handle );
  static native int getDeclType( int handle, int pos );
  static native String getDeclName( int handle, int pos );
  static native int getDeclLine( int handle, int pos );
  static native int getDeclColumn( int handle, int pos );
  static native int getTypeSigIdentifierCount( int handle, int pos );
  static native String getTypeSigIdentifier( int handle, int declPos, int pos );
  static native int getClassDeclMemberCount( int handle, int pos );
  static native boolean isTypeSigMember( int handle, 
                                         int declPos, 
                                         int memberPos );
  static native int getClassDeclTypeSigLine( int handle, int declPos, int pos );
  static native int getClassDeclTypeSigColumn( int handle, 
                                               int declPos, 
                                               int pos );
  static native int getClassDeclTypeSigIdentifierCount( int handle, 
                                                        int declPos, 
                                                        int pos );
  static native String getClassDeclTypeSigIdentifierName( int handle, 
                                                          int declPos, 
                                                          int typeSigPos, 
                                                          int pos );
  static native int getInfixDeclPrecedenceLevel( int handle, int declPos );
  static native int getInfixDeclAssociativity( int handle, int declPos );
  static native int getInfixDeclOperatorCount( int handle, int declPos );
  static native String getInfixDeclOperator( int handle, int declPos, int pos );
  static native int getDataDeclConstructorCount( int handle, int declPos );
  static native String getDataDeclConstructorName( int handle, 
                                                   int declPos, 
                                                   int constPos );
  static native int getDataDeclConstructorLine( int handle, 
                                                int declPos, 
                                                int constPos );
  static native int getDataDeclConstructorColumn( int handle, 
                                                  int declPos, 
                                                  int constPos );
  static native int getFunctionBindingMatchCount( int handle, int declPos );
  static native String getFunctionBindingMatchName( int handle, 
                                                    int declPos, 
                                                    int matchPos );
  static native int getFunctionBindingMatchLine( int handle, 
                                                 int declPos, 
                                                 int matchPos );
  static native int getFunctionBindingMatchColumn( int handle, 
                                                   int declPos, 
                                                   int matchPos );

  // get and dispose stable pointer to a compilation unit's parse result
  private static native int haskellParseCU( String content );
  private static native String getErrorMessage( int handle );
  private static native void haskellFreeHandle( int handle );
  // drive the Haskell runtime system
  private static native void haskellInit();
  private static native void haskellExit();
}